// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases WaitNotifyTestCode
//:: tools
class Test {
  public static void main(String args[]){
    Main main=new Main();
    System.err.printf("q1 == %d%n",main.q1.data);
    System.err.printf("q2 == %d%n",main.q2.data);
    main.main();
    System.err.printf("q1 == %d%n",main.q1.data);
    System.err.printf("q2 == %d%n",main.q2.data);
  }  
}

