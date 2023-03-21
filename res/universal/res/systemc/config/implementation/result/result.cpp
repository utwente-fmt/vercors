


class sc_mutexx : public sc_prim_channel
{
public:

    sc_event _free;

    bool _locked;

    // blocks until mutex could be locked
    void lock()
    {
      while( _locked ) {
 wait( _free );
      }

      _locked = true;
    }

    // returns false if mutex could not be locked
    bool trylock()
    {
      if( _locked )
  return false;
      else
 return true;
    }

    // unlocks mutex
     void unlock()
    {
      _locked = false;
      _free.notify();
    }

   // constructor
    sc_mutexx(){
      //      _locked = false;
    }

};

class sc_fifox : sc_prim_channel{
  private:
    int size;
    int buf[16];
    int free;
    int ri;
    int wi;

    int num_readable; // #samples readable
    int num_read; // #samples read during this delta cycle
    int num_written;

    sc_event data_read_event;
    sc_event data_written_event;


  public:

    sc_fifox() {
      size = 16;
      free = 16;
      ri = 0;
      wi = 0;

      num_readable = 0;
      num_read = 0;
      num_written = 0;
    }

    int read() {
      int val;
      while( num_readable - num_read == 0 ) {
        wait( data_written_event );
      }
      num_read = num_read + 1;

      val = buf[ri];
      ri = ( ri + 1 ) % size;
      free = free + 1;
      request_update();
      return val;
    }

    void write(int val) {
      while( size - num_readable - num_written == 0 ) {
        wait( data_read_event );
      }
      num_written = num_written + 1;

      buf[wi] = val;
      wi = ( wi + 1 ) % size;
      free = free -1;

      request_update();
    }



    void update()
    {
      if( num_read > 0 ) {
        data_read_event.notify(SC_ZERO_TIME);
      }

      if( num_written > 0 ) {
        data_written_event.notify(SC_ZERO_TIME);
      }

      num_readable = size - free;
      num_read = 0;
      num_written = 0;
    }


    int num_available() {
      return ( num_readable - num_read );
    }

    int num_free() {
      return ( size - num_readable - num_written );
    }

};
/*

class myfifo_if : 
    virtual public sc_interface 
{
  public:
    virtual void write(int) = 0;
    virtual void read(int &) = 0;
};

class myfifo : 
    public sc_prim_channel, public myfifo_if 
{
  private:
    int buffer[BUFFERSIZE];
    int n_free, n_read, n_written, r_pos, w_pos;
  public: 
    sc_event w_event, r_event;
  myfifo(const char* name) : 
      sc_prim_channel(name), n_free(0), 
  r_pos(0), w_pos(0), n_read(0), n_written(0) {}  

  void write(int c) 
  {
    while (n_free == 0)
      wait(r_event);
    buffer[w_pos] = c;
    w_pos = (w_pos + 1)%BUFFERSIZE;
    n_written++;
    n_free--;
    request_update();
  }

  void read(int &c) 
  {
    while (n_free == BUFFERSIZE)
    {
      cout << sc_time_stamp() << ": read is waiting for w_event" << endl;
      wait(w_event);
    }
    n_read++;
    c = buffer[r_pos];
    r_pos = (r_pos + 1)%BUFFERSIZE;
    n_free++;
    request_update();
  }

  void update()
  {
    if( n_read > 0 ) {
      r_event.notify(SC_ZERO_TIME);
    }

    if( n_written > 0 ) {
      w_event.notify(SC_ZERO_TIME);
    }

    n_read = 0;
    n_written = 0;
  }

};

*/



class sc_signalx : public sc_prim_channel
{
  public:
    sc_event change;

    int val;
    int _val;

    int delta;


    int read() {
      return val;
    }

    void write(int newval) {
      _val = newval;
      if (!(_val == val))
 request_update();
    }

    void update() {
      if (!(_val == val))
 {
   val = _val;
   change.notify(SC_ZERO_TIME);
   delta = delta_count();
 }
    }

    bool event() {
      return simcontext()->event_occurred(delta);
    }

    sc_signalx() {
      delta = -1;
    }

    /*
    void default_event() { return change };
    */
};


struct data
{
 int val;
};
class tlm_fw_bw_if : public sc_prim_channel
{
private:
  //  int dummy;
  // struct data
  // {
  // 	containing the submitted value
  //   int val;
  // };
  //  public:
  //  int test() {return 0;}
  virtual void invalidate_direct_mem_ptr(sc_dt::uint64 start_range, sc_dt::uint64 end_range) {}
  virtual int nb_transport_bw(data& tran, int& phase, sc_time& t) {}
  virtual int nb_transport_fw(data& tran, int& phase, sc_time& t) {}
  virtual void b_transport(data& tran, sc_time& t) {}
  virtual bool get_direct_mem_ptr(data& tran, tlm::tlm_dmi& dmi_data) { return false; }
  virtual unsigned int transport_dbg(data& tran) { return 0; }
};





class sc_mutexx : public sc_prim_channel
{
public:

    sc_event _free;

    bool _locked;

    // blocks until mutex could be locked
    void lock()
    {
      while( _locked ) {
 wait( _free );
      }

      _locked = true;
    }

    // returns false if mutex could not be locked
    bool trylock()
    {
      if( _locked )
  return false;
      else
 return true;
    }

    // unlocks mutex
     void unlock()
    {
      _locked = false;
      _free.notify();
    }

   // constructor
    sc_mutexx(){
      //      _locked = false;
    }

};


class sc_fifox : sc_prim_channel{
  private:
    int size;
    int buf[16];
    int free;
    int ri;
    int wi;

    int num_readable; // #samples readable
    int num_read; // #samples read during this delta cycle
    int num_written;

    sc_event data_read_event;
    sc_event data_written_event;


  public:

    sc_fifox() {
      size = 16;
      free = 16;
      ri = 0;
      wi = 0;

      num_readable = 0;
      num_read = 0;
      num_written = 0;
    }

    int read() {
      int val;
      while( num_readable - num_read == 0 ) {
        wait( data_written_event );
      }
      num_read = num_read + 1;

      val = buf[ri];
      ri = ( ri + 1 ) % size;
      free = free + 1;
      request_update();
      return val;
    }

    void write(int val) {
      while( size - num_readable - num_written == 0 ) {
        wait( data_read_event );
      }
      num_written = num_written + 1;

      buf[wi] = val;
      wi = ( wi + 1 ) % size;
      free = free -1;

      request_update();
    }



    void update()
    {
      if( num_read > 0 ) {
        data_read_event.notify(SC_ZERO_TIME);
      }

      if( num_written > 0 ) {
        data_written_event.notify(SC_ZERO_TIME);
      }

      num_readable = size - free;
      num_read = 0;
      num_written = 0;
    }


    int num_available() {
      return ( num_readable - num_read );
    }

    int num_free() {
      return ( size - num_readable - num_written );
    }

};
/*

class myfifo_if : 
    virtual public sc_interface 
{
  public:
    virtual void write(int) = 0;
    virtual void read(int &) = 0;
};

class myfifo : 
    public sc_prim_channel, public myfifo_if 
{
  private:
    int buffer[BUFFERSIZE];
    int n_free, n_read, n_written, r_pos, w_pos;
  public: 
    sc_event w_event, r_event;
  myfifo(const char* name) : 
      sc_prim_channel(name), n_free(0), 
  r_pos(0), w_pos(0), n_read(0), n_written(0) {}  

  void write(int c) 
  {
    while (n_free == 0)
      wait(r_event);
    buffer[w_pos] = c;
    w_pos = (w_pos + 1)%BUFFERSIZE;
    n_written++;
    n_free--;
    request_update();
  }

  void read(int &c) 
  {
    while (n_free == BUFFERSIZE)
    {
      cout << sc_time_stamp() << ": read is waiting for w_event" << endl;
      wait(w_event);
    }
    n_read++;
    c = buffer[r_pos];
    r_pos = (r_pos + 1)%BUFFERSIZE;
    n_free++;
    request_update();
  }

  void update()
  {
    if( n_read > 0 ) {
      r_event.notify(SC_ZERO_TIME);
    }

    if( n_written > 0 ) {
      w_event.notify(SC_ZERO_TIME);
    }

    n_read = 0;
    n_written = 0;
  }

};

*/




class sc_signalx : public sc_prim_channel
{
  public:
    sc_event change;

    int val;
    int _val;

    int delta;


    int read() {
      return val;
    }

    void write(int newval) {
      _val = newval;
      if (!(_val == val))
 request_update();
    }

    void update() {
      if (!(_val == val))
 {
   val = _val;
   change.notify(SC_ZERO_TIME);
   delta = delta_count();
 }
    }

    bool event() {
      return simcontext()->event_occurred(delta);
    }

    sc_signalx() {
      delta = -1;
    }

    /*
    void default_event() { return change };
    */
};



struct data
{
 int val;
};
class tlm_fw_bw_if : public sc_prim_channel
{
private:
  //  int dummy;
  // struct data
  // {
  // 	contain



class sc_clockx : sc_module
{
  public:
    sc_event edge;

  private:
    int period;

    SC_HAS_PROCESS(sc_clockx);

    void run(void) {
      int tmp = period/2;
      edge.notify(SC_ZERO_TIME);
        while(true) {
            wait(tmp, SC_NS);
     edge.notify(SC_ZERO_TIME);
        }
    }

  public:
    sc_clockx(sc_module_name name, int periodparam): sc_module(name)
    {
        SC_THREAD(run);
        period = periodparam;
    }

};
