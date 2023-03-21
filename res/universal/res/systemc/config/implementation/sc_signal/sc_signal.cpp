#include <systemc.h>

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
	  delta = sc_delta_count();
	}
    }
            
    bool event() {
      return delta == sc_delta_count();
    } 
    
    sc_signalx() {
      delta = -1; 
    }
    
    /*
    void default_event() { return change };
    */
};
