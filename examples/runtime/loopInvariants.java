class Program {

    public int indexOf(int[] a, int b) {

        //@ loop_invariant i <= a.length;
        //@ loop_invariant \forall* int j; 0 <= j < a.length; Perm(a[j], read);
        for(int i = 0; i < a.length; i++) {
            if(a[i] == b) {
                return i;
            }
        }
        return -1;
    }

    public static void main(String[] args) {
        Program main = new Program();
        main.indexOf(new int[]{1,2,3,4,5}, 4);
    }
}