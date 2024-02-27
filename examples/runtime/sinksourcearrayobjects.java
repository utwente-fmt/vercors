class DummyData {
    int val;
}

class Source {
    DummyData[] i;

    public void createI() {
        this.i = new DummyData[2];
        DummyData o = new DummyData();
        this.i[0] = o;
        this.i[1] = o;
    }

    /*@
        requires Perm(this.i, 1);
        requires (\forall* int j; 0 <= j && j < i.length; Perm(i[j].val, write));
        ensures Perm(this.i, 1);
        ensures (\forall* int j; 0 <= j && j < i.length; Perm(i[j].val, write));
     */
    public void test() {

    }

    public void main(){
        Source source = new Source();
        source.createI();
        source.test();
    }
}
