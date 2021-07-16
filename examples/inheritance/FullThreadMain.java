// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases FullThread
//:: tools
// takes too much time to wait for...
//  :: tools chalice
//  :: options --explicit
//:: verdict Pass

/*
  vct --chalice FullThreadInstance.java FullThread.java FullThreadMain.java
*/
public class Main {


    public void direct() {
        //@ witness res:Instance.postJoin(*);
        //  TODO: crash instead of error: Instance.postJoin.instance inst;
        //@ witness res_inst:Instance.postJoin@Instance(100);
        Instance i = new Instance(7)/*@ label inst */;
        //@ assert i.input==7;
        doit:
        i.run() /*@ with { pre = inst.pre; } */;
        //@ res=doit.post ;
        //@ assert i.input==7;

        //@ open res:i.postJoin@Instance(100) { label dummy1 with { family = res ; class_of = inst.class_of ; } then { res_inst = member ; } } ;
        //@ unfold res_inst:i.postJoin@Instance(100);

        //@ assert i.output==8;
    }

    public void forkjoin() {
        //@ witness res:Instance.postJoin(*);
        //@ witness res_inst:Instance.postJoin@Instance(100);
        Instance i = new Instance(7)/*@ label inst */;
        //@ assert i.input==7;
        queue:
        i.start() /*@ with { p = 100 ; pre = inst.pre;  } */;
        sync:
        i.joinWith() /*@ with { p = 100 ; tok = queue.tok; } */;
        //@ assert i.input==7;

        //@ res=sync.post ;

        //@ open res:i.postJoin@Instance(100) { label dummy2 with { family = res ; class_of = inst.class_of ; } then { res_inst = member ; } } ;
        //@ unfold res_inst:i.postJoin@Instance(100);

        //@ assert i.output==8;
    }

}

