class Program {
    int[] a;

    public void setA(int[] a) {
        this.a = a;
    }

    public int indexOf(int b) {
        //@ loop_invariant i <= a.length;
        //@ loop_invariant (\forall* int j; 0 <= j && j < a.length; Perm(a[j], read));
        //@ loop_invariant (\forall int j; 0 <= j && j < a.length; a[j] % 3 == 0);
        for(int i = 0; i < a.length; i++) {
            if(a[i] == b) {
                return i;
            }
        }
        return -1;
    }

    public void main() {
        Program program = new Program();
        int[] tmp = new int[3];
        tmp[0] = 2;
        tmp[1] = 4;
        tmp[2] = 6;
        program.setA(tmp);
        program.indexOf(4);
    }
}