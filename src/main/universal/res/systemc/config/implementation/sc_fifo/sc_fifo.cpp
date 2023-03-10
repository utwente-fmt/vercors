class sc_fifox : sc_prim_channel{
  private:
    static const int BUF_SIZE = 16;
    int size;
    char buf[BUF_SIZE];
    int free;
    int ri;
    int wi;
    
    int num_readable;         // #samples readable
    int num_read;             // #samples read during this delta cycle
    int num_written;
    
    sc_event data_read_event;
    sc_event data_written_event;

  
  public:
    
    sc_fifox() {
      size = BUF_SIZE;
      free = BUF_SIZE;
      ri = 0;
      wi = 0;

      num_readable = 0;
      num_read = 0;
      num_written = 0;
    }
     
    char read() {
      char val;
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
    
    char nb_read(char &val) {
      if( num_readable - num_read == 0 ) {
        return false;
      }
      num_read = num_read + 1;
       
      val = buf[ri];
      ri = ( ri + 1 ) % size;
      free = free + 1;
      request_update();
      return true;
    }        

    void write(char val) {
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
#define BUFFERSIZE 3

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
