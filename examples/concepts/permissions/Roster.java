// From page 42 of Hurlin's PhD thesis: <https://theses.hal.science/tel-00424979v1>

final class Roster {
  int id;
  int grade;
  Roster next;

  /*@
    resource ids_and_links() = Perm(id,1) ** Perm(next,1\2) ** next->ids_and_links();

    resource grades_and_links() = Perm(grade,1) ** Perm(next,1\2) ** next->grades_and_links() ;

    resource state() = ids_and_links() ** grades_and_links();
  */

  //@ requires n->state();
  //@ ensures this.state();
  Roster(int i, int g, Roster n) {
    id = i;
    grade = g;
    next = n;
    /*@ ghost {
      if (n!=null) {
        unfold n.state(); 
      }
    } */
    //@ fold ids_and_links();
    //@ fold grades_and_links();
    //@ fold state();
  }

  //@ given frac q;
  //@ requires 0 < q && q < 1;
  //@ requires grades_and_links() ** Perm(ids_and_links(), q);
  //@ ensures grades_and_links() ** Perm(ids_and_links(), q);
  void updateGrade(int id, int grade) {
    /*@
      unfold Perm(ids_and_links(), q);
      unfold grades_and_links();
    @*/
    if (this.id == id) {
      this.grade = grade;
    } else if (next != null) {
      next.updateGrade(id,grade) /*@ given { q = q\2 } */;
    }
    /*@
      fold Perm(ids_and_links(), q);
      fold grades_and_links();
    @*/
  }

  //@ given frac q;
  //@ requires 0 < q && q < 1;
  //@ requires Perm(ids_and_links(), q);
  //@ ensures Perm(ids_and_links(), q);
  boolean contains(int id) {
    //@ unfold Perm(ids_and_links(), q);
    boolean b = this.id==id;
    if(!b && next!=null){
      b=next.contains(id) /*@ given { q = q \ 2 } */;
    }
    //@ fold Perm(ids_and_links(), q);
    return b;
  }
}

