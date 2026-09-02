/* Example:
	SingleCell
 Description:
	AtomicInteger is used to protect data field of SingleCell.
	Multiple threads trying to assign or find their value.
 Original Author:
	Afshin Amighi
*/

public class SingleCellMod{
	/*@
	 ghost resource inv_value(int x) = (x ==0 ==> true) ** ( x== 1 ==> true ) ** (x == 2 ==> Value(data));
	 ghost resource inv_resource(int x) = (x == 0 ==> Perm(data,1));
	 */

	/*@ ghost resource contains(int d)= Value(data) ** data == d; */
	private int data;

	public SingleCellMod() { this.data = -1; }

	/*@
	 requires (inv_resource(n)) ** inv_value(n);
	 ensures inv_value(n);
	 */
	void set(int n);

	/*@
	 requires  true;
	 ensures ((\result ==> inv_value(n) ) ** (!\result ==> true)) ;
	 */
	boolean equal(int n);

	/*@
	 requires	(inv_resource(n)) ** inv_value(n);
	 ensures	(\result ==> (inv_resource(o)) ** inv_value(o) ) **
				(!\result ==> (inv_resource(n)) ** inv_value(n));
	 */
	boolean cas(int o, int n);



	/*@
		requires inv_value(1);
		requires inv_resource(1);
		ensures \result == 0 ==> (inv_value(2) ** contains(d));
		ensures \result == 1 ==> (inv_value(2) ** contains(d));
	*/
	public int find_or_put(int d){
		boolean b;

		b= cas(0,1);

		if(true && b){

			// unfold cas postcondition to get the write permission
			/*@
			 unfold inv_resource(0);
			 */

			// write
			data = d;

			// data contains the value, build contains predicate
			// and provide set postconditions

			/*@
			 fold contains(d);

			 fold inv_value(2);
			 fold inv_resource(2);
			 */

			set(2);


			return 0;

		}
		if (!b) {
			boolean spin = false;
			/*@ loop_invariant true; */

			while(!spin) {
				spin = equal(1);
			}
			boolean check;

			check = ( equal(2) );

			if (true && check ){

				/*@ unfold inv_value(2); */
				if (true && data == d) {
					/*@
					 fold inv_value(2);
					 fold contains(d);
					 */

					return 1;
				}
				/*@ fold inv_value(2); */

				return -2;//collision
			}
		}

		return -1;//error
	}

}
