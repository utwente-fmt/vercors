// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Roster
//:: tools chalice
//:: options --explicit
//:: suite medium

/**
 * See pg 42, phd Hurlin.
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --chalice --explicit Roster.java
 * <p>
 * The expected result is Pass.
 */
class Roster {
    int id;
    int grade;
    Roster next;

/*@
  resource ids_and_links(frac p,frac q)=Perm(id,p)
    ** Perm(next,q) ** (next!=null ==> idal:next.ids_and_links(p,q));
   
  resource grades_and_links(frac p,frac q)=Perm(grade,p)
    ** Perm(next,q) ** (next!=null ==> gral:next.grades_and_links(p,q));
     
  resource state(frac p) =
    (idal:ids_and_links(p,p/2)) ** (gral:grades_and_links(p,p/2));
     
*/

    //@ requires n!=null ==> state_in:n.state(100);
    //@ ensures  state_out:state(100);
    public Roster(int i, int g, Roster n) {
        this.id = i;
        this.grade = g;
        this.next = n;
    /*@
    witness tmp1:grades_and_links(*,*);
    witness tmp2:Roster.ids_and_links(*,*);
    if (n!=null) {
      unfold state_in:n.state(100);
      fold tmp1:grades_and_links(100,50,gral:state_in.gral);
      fold tmp2:ids_and_links(100,50,idal:state_in.idal);
    } else {
      fold tmp1:grades_and_links(100,50,gral:null);
      fold tmp2:ids_and_links(100,50,idal:null);
    }
    fold state_out:state(100,idal:tmp2,gral:tmp1);
    @*/
    }

    /*@
      given frac p, q, r;
      requires gral1:grades_and_links(100,p);
      requires idal1:ids_and_links(q,r);
      ensures  gral2:grades_and_links(100,p);
      ensures  idal2:ids_and_links(q,r);
     */
    void updateGrade(int id, int g) {
        //@ unfold gral1:grades_and_links(100,p);
        //@ unfold idal1:ids_and_links(q,r);
        //@ witness gral_tmp:grades_and_links(*,*);
        //@ witness idal_tmp:ids_and_links(*,*);
        //@ gral_tmp=gral1.gral;
        //@ idal_tmp=idal1.idal;
        if (this.id == id) {
            grade = g;
        } else if (next != null) {
            rec1:
            next.updateGrade(id, g)
      /*@ with {
        p=p;
        q=q;
        r=r;
        gral1=gral1.gral;
        idal1=idal1.idal;
      } then {
        gral_tmp = gral2;
        idal_tmp = idal2;
      } */;
        }
        //@ fold gral2:grades_and_links(100,p,gral:gral_tmp);
        //@ fold idal2:ids_and_links(q,r,idal:idal_tmp);
    }

    /*@
      given frac q,r;
      requires idal1:ids_and_links(q,r);
      ensures  idal2:ids_and_links(q,r);
     */
    boolean contains(int id) {
        //@ unfold idal1:ids_and_links(q,r);
        //@ witness idal_tmp:ids_and_links(*,*);
        //@ idal_tmp=idal1.idal;
        boolean b = this.id == id;
        if (!b && next != null) {
            b = (next.contains(id)
      /*@ with {q=q;r=r;idal1=idal1.idal; }
        then { idal_tmp = idal2; } */);
        }
        //@ fold idal2:ids_and_links(q,r,idal:idal_tmp);
        return b;
    }
}

