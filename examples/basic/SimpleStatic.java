//:: case SimpleStatic
//:: tools silicon
//:: verdict Pass

public class Example {
    /*@
       context_everywhere ar != null;
       requires Perm(ar[*], 1);
    */
    public static void example(int[] ar) {
        for (int i = 0; i < ar.length; i++)
        /*@
           requires Perm(ar[i], 1);
        */
        {
            ar[i] = 0;
        }
    }
}