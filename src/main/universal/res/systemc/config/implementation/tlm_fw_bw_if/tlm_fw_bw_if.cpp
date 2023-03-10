
struct data
{
	int val;
};
class tlm_fw_bw_if : public sc_interface
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
    
 
