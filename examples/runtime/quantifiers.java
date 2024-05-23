class Program {
    int[] a;
    int b;


    /*@
        requires (\forall* int i; 0 <= i && i < a.length; Perm(a[i], write));
     */
    public void setA(int[] a) {
        this.a = a;
    }

    /*@
        requires a.length > 0;
        requires (\forall* int x; 0 <= x && x < a.length; Perm(a[x], write));
        requires (\forall int x; 0 <= x && x < a.length; a[x] > 0);
        requires (\forall int x; 0 <= x && x < a.length; (\forall int j; 0 <= j && j < x; a[j] >= a[x]));
        ensures (\forall* int x; 0 <= x && x < a.length; Perm(a[x], write));
        ensures (\exists int x; 0 <= x && x < a.length; a[x] > 0);
     */
    //ensures (\exists int x; 0 <= x && x < a.length; a[x] == b) ==> \result >= 0;
    public int indexOf(int b) {
        for (int i = 0; i < a.length; i++) {
            if (a[i] == b) {
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