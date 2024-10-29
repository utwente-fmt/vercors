import subprocess
import sys
import re
from collections import defaultdict
import json
import random

CALL_RE = re.compile(r"^\[pid ([0-9]+)\] ([_a-z]+)\(([^)]+)\)")

STATE_UNDECIDED = 0
STATE_NOT_MILL = 1
STATE_MILL = 2

BUF_LIMIT = 1024 * 16
STDIN = 0
STDOUT = 1
STDERR = 2
STREAMS = [STDIN, STDOUT, STDERR]

STRACE = [
    "strace",
    "-X", "raw", # always display raw number output instead of labels/structs
    "-e", "verbose=read,write", # decode strings for read and write syscalls
    "-e", "trace=read,write", # trace only read/write calls
    "-e", "status=successful,failed", # filter unfinished calls
    "-s", str(1024**2), # increase maximum decoded string length
    "-xx", # print all string characters as hex
    "-f", # follow new child processes (such as mill)
]

OTHER_FILE_ID = 0

def pipe_of_pid_fd(pid, fd):
    global OTHER_FILE_ID
    try:
        pipe = subprocess.check_output(["readlink", f"/proc/{pid}/fd/{fd}"])
        if pipe.startswith(b"pipe:["):
            return pipe
        else:
            OTHER_FILE_ID += 1
            return f"[other:{OTHER_FILE_ID}]".encode('utf-8')
    except subprocess.CalledProcessError:
        print(f"pid {pid} fd {fd}: could not query pipe before process exit")
        OTHER_FILE_ID += 1
        return f"[other:{OTHER_FILE_ID}]".encode('utf-8')

def unhex(literal):
    assert literal.startswith('"') and literal.endswith('"'), literal
    data = literal[1:-1].replace('\\x', '')
    return bytes.fromhex(data)


BSP_ID = 0


class BspState:
    def __init__(self):
        global BSP_ID
        BSP_ID += 1
        self.id = BSP_ID
        self.stdin_dbg = b""
        self.stdin_buf = b""
        self.stdin_who = {}

        self.stdout_dbg = b""
        self.stdout_buf = b""
        self.stdout_who = {}

        self.requests = {}

    def messages(self, buf, who):
        ret = []
        old_buf = buf

        try:
            while b"\r\n\r\n" in buf:
                work_buf = buf
                headers, work_buf = work_buf.split(b"\r\n\r\n", maxsplit=1)
                read_here = len(headers) + len(b"\r\n\r\n")
                headers = headers.split(b"\r\n")
                headers = (header.split(b":", maxsplit=1) for header in headers)
                headers = {k.decode('utf-8').lower(): v for k, v in headers}
                length = int(headers["content-length"].decode('utf-8').strip())

                if length > len(work_buf):
                    return buf, ret, who

                data = work_buf[:length]
                work_buf = work_buf[length:]
                read_here += length

                who_here = set()
                new_who = {}
                for fr, pid in who.items():
                    if fr < read_here:
                        who_here.add(pid)
                    else:
                        new_who[fr - read_here] = pid

                who = new_who
                buf = work_buf

                ret.append((who_here, json.loads(data)))
        except Exception as e:
            print()
            print(f"Broken jsonrpc message stream! {e} Started from:")
            print(old_buf.decode('utf-8'))
            print("Decoded:")
            print(ret)
            print("Processing near:")
            print(buf.decode('utf-8') or "(end)")
            print(who)

            recover_from = buf.find(b"Content-Length")

            return b"" if recover_from == -1 else buf[recover_from:], ret, {}

        return buf, ret, who

    def pplines(self, value):
        if isinstance(value, dict):
            items = [(k, self.pplines(v)) for k, v in value.items()]

            if all(len(v) == 1 for _, v in items) and sum(len(k) + len(v[0]) + 5 for k, v in items) < 120:
                return ["{" + ", ".join(f"{k} = {v[0]}" for k, v in items) + "}"]
            else:
                res = []
                res.append("{")
                for k, v in items:
                    res.append("  " + k + " = " + v[0])
                    for line in v[1:]:
                        res.append("  " + line)
                    res[-1] = res[-1] + ","
                res.append("}")
                return res

        elif isinstance(value, list):
            items = [self.pplines(v) for v in value]
            if all(len(v) == 1 for v in items) and sum(len(v[0]) + 2 for v in items) < 120:
                return ["[" + ", ".join(v[0] for v in items) + "]"]
            else:
                res = []
                res.append("[")
                for v in items:
                    for line in v:
                        res.append("  " + line)
                    res[-1] = res[-1] + ","
                res.append("]")
                return res
        else:
            text = repr(value)
            if len(text) > 120:
                text = text[:60] + " ... " + text[-60:]
            return [text]

    def pp(self, value):
        return "\n".join(self.pplines(value))

    def print_msg(self, message):
        if "error" in message or "result" in message:
            if "error" in message:
                res_message = f"error {message['error']['code']}: {message['error'].get('message', '')}"
            else:
                res_message = self.pp(message['result'])

            if message['id'] in self.requests:
                request = self.requests[message['id']]
                del self.requests[message['id']]
                return f"{request['method']}#{request['id']}(...) = {res_message}"
            else:
                return f"#{request['id']} = {res_message}"
        elif "id" in message:
            self.requests[message['id']] = message
            return f"{message['method']}#{message['id']}({self.pp(message['params'])})"
        else:
            return f"notification: {message['method']}({self.pp(message['params'])})"

    def process_input(self):
        self.stdin_buf, messages, self.stdin_who = self.messages(self.stdin_buf, self.stdin_who)
        for pid, message in messages:
            print(f"BS {self.id} from {pid} < {self.print_msg(message)}")
        pass

    def process_output(self):
        self.stdout_buf, messages, self.stdout_who = self.messages(self.stdout_buf, self.stdout_who)
        for pid, message in messages:
            print(f"BS {self.id} from {pid} > {self.print_msg(message)}")

        if self.requests:
            unanswered = [f"{request['method']}#{request['id']}(...)" for request in self.requests.values()]
            print(f"BS {self.id}: not answered yet: {unanswered}")

    def add_input(self, data, who):
        self.stdin_who[len(self.stdin_buf)] = who
        self.stdin_buf += data
        self.stdin_dbg += data
        self.process_input()

    def add_output(self, data, who):
        self.stdout_who[len(self.stdout_buf)] = who
        self.stdout_buf += data
        self.stdout_dbg += data
        self.process_output()

    def add_err(self, data):
        print(f"BS {self.id}: {data.decode('utf-8')}")


def main():
    p = subprocess.Popen(STRACE + ["-p", sys.argv[1]], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    pid_pipes = {}
    pipe_stream = {}
    pipe_bsp_state = {}

    for line in iter(p.stderr.readline, b""):
        line = line.decode('ascii')
        match = CALL_RE.match(line)
        if not match:
            continue

        pid, id, args = match.groups()

        if pid not in pid_pipes:
            pid_pipes[pid] = {}
            for stream in STREAMS:
                pipe = pipe_of_pid_fd(pid, stream)
                pid_pipes[pid][stream] = pipe
                pipe_stream[pipe] = bytearray()

        args = args.split(", ")

        if len(args) != 3 or id not in {"read", "write"} or args[0] not in (str(stream) for stream in STREAMS):
            continue

        fd, data, _ = args
        fd = int(fd)
        pipe = pid_pipes[pid][fd]
        data = unhex(data)

        if pipe in pipe_bsp_state:
            if fd == STDIN:
                pipe_bsp_state[pipe].add_input(data, pid)
            elif fd == STDOUT:
                pipe_bsp_state[pipe].add_output(data, pid)
            elif fd == STDERR:
                pipe_bsp_state[pipe].add_err(data)
        elif pipe in pipe_stream:
            buf = pipe_stream[pipe]
            buf.extend(data)

            if b"Content-Length:" in pipe_stream[pid_pipes[pid][STDIN]]:
                state = BspState()
                state.add_input(pipe_stream[pid_pipes[pid][STDIN]], -1)
                state.add_output(pipe_stream[pid_pipes[pid][STDOUT]], -1)
                state.add_err(pipe_stream[pid_pipes[pid][STDERR]])

                for stream in STREAMS:
                    pipe = pid_pipes[pid][stream]
                    pipe_bsp_state[pipe] = state
                    del pipe_stream[pipe]

            elif len(buf) > BUF_LIMIT:
                for stream in STREAMS:
                    pipe = pid_pipes[pid][stream]
                    del pipe_stream[pipe]

    p.wait()

if __name__ == "__main__":
    main()