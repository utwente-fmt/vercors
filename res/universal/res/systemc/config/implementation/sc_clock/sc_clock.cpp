#include <systemc.h>

class sc_clockx : sc_module, sc_interface
{
  public:
    sc_event edge;
    sc_event change;

    bool val;
    bool _val;
            
    int delta;

	private:
		int period;

		SC_HAS_PROCESS(sc_clockx);

		void run(void) {
			int tmp = period/2;
			edge.notify(SC_ZERO_TIME);
			change.notify(SC_ZERO_TIME);
			while(true) {
				wait(tmp, SC_NS);
				edge.notify(SC_ZERO_TIME);
				change.notify(SC_ZERO_TIME);
				val = !val;
			}
		}

	public:
		sc_clockx(sc_module_name name, int periodparam): sc_module(name)
	{
		SC_THREAD(run);
		period = periodparam;
		val = true;
		_val = true;
	}

  bool read() {
      return val;
    }
    
  void write(bool newval) {
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
            
};
