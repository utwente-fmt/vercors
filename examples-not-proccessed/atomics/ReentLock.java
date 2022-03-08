 // -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ReentLockWitnesses
//:: suite medium
//:: tools chalice
//:: options --explicit

 /*
 Example: ReentLock
 Description: ReentLock is the re-entrant lock using AtomicInteger as synchronizer.
 The contracts for the AtomicInteger is the version without magic-wand (delta).
 Author: Afshin Amighi
 Status: Pass.
 command: vct --chalice --explicit ReentLock.java
 */


public class ReentLock{

	//shared resource
	private int data;

	//  int SynchronizerRole = 0, ThreadRole = 1;
	//	int UNLOCKED = 0 , LOCKED = threadid;

	/*@
	 resource handle(int role,int val);

	 resource allowed(int role,int last,int next)=( role == 1  ==> true );

	 resource assigned(int role, int val) = (role == 0 && val == 0) ==> (Perm(data,100) ** Perm(count,100));
	 */

	private int		count;

	/*@
	 resource state(int id , int held) = (held > 0 )==> Perm(count,100);
	 */


	/*@
	 given int r, l;
	 requires (srh:handle(r,l)) ** (sra:allowed(r,l,x)) ** (srp:assigned(r,l)) ** (srs:assigned(0,x));
	 ensures (seh:handle(r,x)) ** (sep:assigned(r,x));
	 */
	void set(int x);


	/*@
	 given int r,l;
	 requires (grh:handle(r,l)) ** grp:assigned(r,l);
	 ensures (geh:handle(r,\result)) ** gep:assigned(r,\result);
	 */
	int get();


	// the last ensures is temp just to solve the cas loop. ToDo: do we need it? if we remove it , how to preserve the loop invariant?
	/*@
	 given int r, l;
	 requires (crh:handle(r,l)) ** (cra:allowed(r,o,x)) ** (crp:assigned(r,l)) ** (crs:assigned(0,x));
	 ensures \result ==> (cehp:handle(r,x)) ** (cepp:assigned(r,x)) ** (cesp:assigned(0,o));
	 ensures !\result ==> (cehn:handle(r,l)) ** (cepn:assigned(r,l)) ** (cesn:assigned(0,x));
	 ensures !\result ==> cea:allowed(r,o,x);
	 */
	boolean compareAndSet(int o,int x);


	/*
	 requires Perm(owner,100) * Perm(holds,100) * P ; where P is what is needed for lock constructor
	 ensures Perm(owner,100) * Perm(holds,100) * P * (\forall* handles(T,UNLOCKED));
	 */
	/*ReentLock(){

		state = new AtomicInteger(UNLOCKED);
		// handles for all the threads
		owner = null;
		holds = 0;

	}*/

	/*@
	 given int last;
	 given int held;

	 requires tid > 0;
	 requires lrs: state(tid,held);
	 requires lrh: handle(1,last);

	 ensures les: state(tid,held+1);
	 ensures (held == 0) ==> Perm(data,100) ;
	 ensures leh: handle(1,tid);
	 */


	public void dolock(int tid){
		boolean res = false;

		//@ int role = 1, S=0;

		/*@ witness tgrp:assigned(*,*);
			fold tgrp:assigned(role,last);

		    witness tgep:assigned(*,*);
			witness tgeh:handle(*,*);
		 */

		int curr = get() /*@ with { r=role; l = last; grh = lrh;  grp = tgrp; } then { tgeh = geh; tgep = gep; } */;
		// check re-entrant
		if ( tid == curr ) {
			//@ assume (held > 0);
			//@ assert (held > 0);
			//@ unfold lrs: state(tid,held);
			//@ assert (Perm(count,100));
			//@ assume (held == count);

			count = count+1;

			//@ leh=tgeh;
		}
		// check first-entrant
		if( tid != curr){
			//@ assume (held == 0);
			//@ assert (held == 0);

			boolean succ = false;
			//@ int next = tid;

			/*@
			 witness tcra:allowed(role,0,next);
			 fold tcra:allowed(role,0,next);

			 witness tcrs:assigned(S,next);
			 witness tces:assigned(S,next);
			 witness tcepp:assigned(S,next);
			 fold tcrs:assigned(S,next);
			 */
			//@ loop_invariant  !succ	==> (invhn:handle(role,curr)) ** (invpn: assigned(role,curr)) ** invsn: assigned(S,next);
			//@ loop_invariant  !succ	==>	inva:allowed(role,0,next);
			//@ loop_invariant  succ	==>	(invhp:handle(role,next)) ** (invpp: assigned(role,next)) ** invsp: assigned(S,0);
			while (!succ) /*@ with{ invhn = tgeh; invpn = tgep; invpp = tgep;  invsn = tcrs; inva = tcra; }
						   then { tces = invsp; tcepp = invpp; leh = invhp;   } */ {

				succ = compareAndSet(0,tid) /*@ with{ r = role; l = curr; crh = invhn; crp = invpn; cra = inva; crs = invsn; }
										then{ invhn = cehn; invpn = cepn; invsn = cesn; invhp = cehp; invpp = cepp; invsp = cesp;  inva = cea; } */ ;
			}
			//@ unfold tces: assigned(S,0);
			//@ assert (Perm(count,100) ** Perm(data,100));
			//@ assume (held == count);
			count = count+1;
			//@ assert(Perm(data,100));
		}

		//@ assert Perm(count,100);
		//@ fold les:state(tid,held+1);

		return;
	}


	// unlock is only called with a valid tid (tid > 0) and valid held (held > 0)
	/*@
	 given int held;

	 requires tid > 0;
	 requires held > 0;

	 requires urs: state(tid,held);
	 requires urh:handle(1,tid);
	 requires held > 0 ==> Perm(data,100);

	 ensures held == 1 ==> (ueuh: handle(1,0));
	 ensures held > 1 ==> (uelh: handle(1,tid)) ** Perm(data,100);
	 ensures ues:state(tid,held-1);
	 */
	public void unlock(int tid){

		//@ int role = 1, S=0;
		//@ int last = tid;

		/*@
		 witness tgrp:assigned(*,*);
		 fold tgrp:assigned(role,last);

		 witness tgep:assigned(role,last);
		 witness tgeh:handle(role,last);
		 */

		int curr = get() /*@ with { r=role; l = last; grh = urh;  grp = tgrp; } then { tgeh = geh; tgep = gep; } */;

		// this should be a global invariant
		//@ assume( curr == tid );

		if ( curr == tid) {
			//@ assume (held > 0);
			//@ unfold urs:state(tid,held);
			//@ assume (count == held);

			if (count == 1) {
				//@ assert (held == 1);

				count = count-1;
				//@ assert (count ==0);
				//@ fold ues:state(tid,count);


				//@ witness tsrp:assigned(*,*);
				//@ witness tsrs:assigned(*,*);
				//@ fold tsrp:assigned(role,curr);
				//@ fold tsrs:assigned(S,0);

				//@ witness tsra:allowed(*,*,*);
				//@ fold tsra:allowed(role,curr,0);

				set(0) /*@ with{ r = role; l = curr; srh = tgeh; srp = tsrp; sra = tsra; srs = tsrs; } then { ueuh = seh; } */;
			}
			else{
				if (count > 1) {
				//@ assert (held > 1);
				count = count-1;
				//@ fold ues:state(tid,held-1);
				//@ uelh = tgeh;

				}
			}
		}
	}



}
