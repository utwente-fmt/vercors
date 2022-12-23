import sys
import subprocess


REMOVE_CHECKSAT_EXCEPT_LAST = True
REMOVE_EMPTY_PUSH_POP = True

REMOVAL_STAGES = [[
    "(declare",
    "(define",
    "(assert",
    "(set-option :timeout ",
]]


def result(path):
    res = subprocess.run(["z3", "-smt2", path, "trace=true", "proof=true"], capture_output=True)
    lines = res.stdout.decode('utf-8').strip().split("\n")
    lines = [line for line in lines if line != "success"]
    # print(lines)
    if any(line.startswith("(error") for line in lines):
        return "ERROR"
    else:
        return lines[-1]


in_path = sys.argv[1]
assert in_path.endswith(".smt2")
out_path = in_path[:-len(".smt2")] + "-min.smt2"

initial_result = result(in_path)
print(f"Target: the last line is: {initial_result}")

current_buf = ""
parens = 0
in_comment = False

commands = []

for c in open(in_path, "r").read():
    if c == ";":
        in_comment = True
    elif c == "\n" and in_comment:
        in_comment = False
    elif in_comment:
        pass
    elif c == "(":
        current_buf += "("
        parens += 1
    elif c == ")":
        current_buf += ")"
        parens -= 1
        if parens == 0:
            commands.append(current_buf)
            current_buf = ""
    elif parens > 0:
        if c.isspace():
            if current_buf[-1] != " ":
                current_buf += " "
        else:
            current_buf += c

print(f"Minimizing {len(commands)} commands...")

if REMOVE_CHECKSAT_EXCEPT_LAST:
    last_check_sat_i = None
    for i, command in enumerate(commands):
        if command == "(check-sat)":
            last_check_sat_i = i

    commands = [command for i, command in enumerate(commands) if command != "(check-sat)" or i == last_check_sat_i]

for removals in REMOVAL_STAGES:
    print(f"Removing commands starting with any of: {removals}")
    last_length = None
    while last_length != len(commands):
        if last_length is not None:
            print(f"This round removed {last_length - len(commands)} commands, going again...")

        last_length = len(commands)

        try_remove_i = len(commands)-1
        while try_remove_i >= 0:
            if any(commands[try_remove_i].startswith(prefix) for prefix in removals):
                try_commands = commands[:try_remove_i] + commands[try_remove_i+1:]

                with open(out_path, "w") as f:
                    for command in try_commands:
                        f.write(command)
                        f.write("\n")

                new_result = result(out_path)

                if initial_result == new_result:
                    print(f"[{len(commands)-1}] Removing line {try_remove_i+1}: {commands[try_remove_i]}")
                    commands = commands[:try_remove_i] + commands[try_remove_i + 1:]
                    if try_remove_i < len(commands):
                        continue

            try_remove_i -= 1

        if REMOVE_EMPTY_PUSH_POP:
            try_remove_i = len(commands) - 2
            while try_remove_i >= 0:
                if commands[try_remove_i] == "(push)" and commands[try_remove_i+1] == "(pop)":
                    try_commands = commands[:try_remove_i] + commands[try_remove_i + 2:]

                    with open(out_path, "w") as f:
                        for command in try_commands:
                            f.write(command)
                            f.write("\n")

                    new_result = result(out_path)

                    if initial_result == new_result:
                        print(f"[{len(commands) - 2}] Removing line {try_remove_i+1}, {try_remove_i+2}: {commands[try_remove_i]}, {commands[try_remove_i+1]}")
                        commands = commands[:try_remove_i] + commands[try_remove_i+2:]
                        if try_remove_i < len(commands) - 1:
                            continue

                try_remove_i = min(len(commands) - 2, try_remove_i - 1)

print("All done!")
