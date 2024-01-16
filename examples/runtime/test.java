class Program {

    int[] a;


    public int indexOf(int b) {

        //@ loop_invariant i <= a.length;
        //@ loop_invariant (\forall* int j; 0 <= j && j < a.length; Perm(a[j], read));
        for(int i = 0; i < a.length; i++) {
            if(a[i] == b) {
                return i;
            }
        }
        return -1;
    }

    public void main() {
        Program main = new Program();
    }
}