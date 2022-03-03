// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SingleCellWitnesses
//:: tools chalice
//:: options --explicit
/*
 Example: 
	SingleCell
 Description: 
	AtomicInteger is used to protect data field of SingleCell.
	Multiple threads trying to assign or find their value.
 Author:
	Afshin Amighi
 Command:
    vct --chalice --explicit SingleCell.java
 Status:
	Pass.
 
*/


public class SingleCellMod{


	/*@	 
	 
	 public resource inv_value(int x) = (x ==0 ==> true) ** ( x== 1 ==> true ) ** (x == 2 ==> Value(data));
	 public resource inv_resource(int x) = (x == 0 ==> Perm(data,100));
	 */
		
	/*@ resource contains(int d)= Value(data) ** data == d; */
	private int data;
	
	public SingleCellMod(){ this.data = -1; }
	
	/*@
	 requires (set_rr:inv_resource(n)) ** set_rv:inv_value(n);
	 ensures set_ev:inv_value(n); 
	 */
	void set(int n);
	
	/*@
	 requires  true;
	 ensures ((\result ==> eql_evp:inv_value(n) ) ** (!\result ==> true)) ;
	 */
	boolean equal(int n);
	
	/*@
	 requires	(cas_rr:inv_resource(n)) ** cas_rv:inv_value(n);
	 ensures	(\result ==> (cas_erp:inv_resource(o)) ** cas_evp:inv_value(o) ) ** 
				(!\result ==> (cas_ern:inv_resource(n)) ** cas_evn:inv_value(n));
	 */
	boolean cas(int o, int n);	
	


	/*@
	 requires true ;
	 ensures \result == 0 ==> (fop_evp:inv_value(2)) ** fop_ecp: contains(d);
	 ensures \result == 1 ==> (fop_evf:inv_value(2)) ** fop_ecf: contains(d);
	 */

	public int find_or_put(int d){
		boolean b;
		// temporary labels
		 /*@ 
		  witness iv_tmp:inv_value(*);
		  witness ir_tmp:inv_resource(*);
		  witness cas_rc_tmp:inv_resource(*);
		  */
		 
		// build cas preconditions
		 /*@ 
		  fold iv_tmp:inv_value(1);
		  fold ir_tmp:inv_resource(1);
		  */
		
		 b= cas(0,1) /*@ with {cas_rv = iv_tmp; cas_rr = ir_tmp; } then { cas_rc_tmp = cas_erp; } */ ;
		
		if(true && b){

			// unfold cas postcondition to get the write permission
			/*@ 
			 unfold cas_rc_tmp:inv_resource(0);
			 */

			// write
			data = d;

			// data contains the value, build contains predicate 
			// and provide set postconditions
			
			/*@ 
			 fold fop_ecp:contains(d);
			 witness im_tmp:inv_value(*);
			 witness rc_tmp:inv_resource(*);

			 fold im_tmp:inv_value(2);
			 fold rc_tmp:inv_resource(2);
			 */
			
			set(2) /*@ with { set_rr = rc_tmp; set_rv=im_tmp; } then { fop_evp = set_ev; } */;


			return 0;
	
		}
		if (!b) {
			boolean spin = false;
			/*@ loop_invariant true; */

			while(!spin) {
				spin = equal(1);
			}
			boolean check;
			
			/*@ witness eql_v_tmp:inv_value(*) ; */
			check = ( equal(2) /*@ with {} then{ eql_v_tmp=eql_evp; } */);
			
			if (true && check ){ 
				
				/*@ unfold eql_v_tmp:inv_value(2); */
				if (true && data == d) {
					/*@ 
					 fold eql_v_tmp:inv_value(2); 
					 fop_evf = eql_v_tmp;
					 fold fop_ecf:contains(d);
					 */

					return 1;
				}
				/*@ fold eql_v_tmp:inv_value(2); */

				return -2;//collision
			}
		}
		
		return -1;//error
	}
}
