package java.lang;
// Blatantly incomplete

import java.io.PrintStream;
import java.io.InputStream;

class System {
    public static final PrintStream out;
    public static final PrintStream err;
    public static final InputStream in;

    /*@
    ghost
    ensures out != null ** err != null;
    public static void staticInvariant() {
        assume false;
    }
    @*/

    //@ requires in != null;
    public static void setIn(InputStream in) {
        //@ assume false;
    }

    //@ requires out != null;
    public static void setOut(PrintStream out) {
        //@ assume false;
    }

    //@ requires err != null;
    public static void setErr(PrintStream err) {
        //@ assume false;
    }
    
    /*@
        context_everywhere src != null ** dest != null;
        context_everywhere length >= 0;
        context_everywhere 0 <= srcPos && srcPos + length <= src.length;
        context_everywhere 0 <= destPos && destPos + length <= dest.length;
        context_everywhere (\forall* int i = srcPos .. srcPos + length; Perm({:src[i]:}, 1\2));
        context_everywhere src == dest && srcPos <= destPos && srcPos + length > destPos ==>
            // srcPos is below destPos, but there is some overlap
            (\forall* int i = destPos .. srcPos + length; Perm({:dest[i]:}, 1\2)) **
            (\forall* int i = srcPos + length .. destPos + length; Perm({:dest[i]:}, write));
        context_everywhere src == dest && srcPos <= destPos && srcPos + length <= destPos ==>
            // srcPos is below destPos, no overlap
            (\forall* int i = destPos .. destPos + length; Perm({:dest[i]:}, write));
        context_everywhere src == dest && srcPos > destPos && destPos + length > srcPos ==>
            // srcPos is above destPos, overlap
            (\forall* int i = destPos .. srcPos; Perm({:dest[i]:}, write)) **
            (\forall* int i = srcPos .. destPos + length; Perm({:dest[i]:}, 1\2));
        context_everywhere src == dest && srcPos > destPos && destPos + length <= srcPos ==>
            // srcPos is above destPos, no overlap
            (\forall* int i = destPos .. destPos + length; Perm({:dest[i]:}, write));
        context_everywhere src != dest ==> (\forall* int i = destPos .. destPos + length; Perm({:dest[i]:}, write));
        ensures (\forall int i = destPos .. destPos + length; {:dest[i]:} == \old(src[srcPos + (i - destPos)]));
        signals (NullPointerException e) dest == null || src == null;
        signals (IndexOutOfBoundsException e) srcPos < 0 || destPos < 0 || length < 0 || srcPos + length > src.length || destPos + length > dest.length;
    @*/
    public static /*native*/ void arraycopy(Object[] src,  int  srcPos,
                                        Object[] dest, int destPos,
                                        int length) {
        if (dest == null || src == null) throw new NullPointerException();
        // left out type checks for src and dest arrays
        if (srcPos < 0 || srcPos + length > src.length || 
            destPos < 0 || destPos + length > dest.length ||
            length < 0) { 
            throw new IndexOutOfBoundsException();
        }
        // left out conversion checks for individual array elements
        
        if (srcPos >= destPos) {
            /*@
            loop_invariant 0 <= i && i <= length;
            loop_invariant (\forall int j = srcPos + i .. srcPos + length; src[j] == \old(src[j]));
            loop_invariant (\forall int j = destPos .. destPos + i; {:dest[j]:} == \old(src[srcPos + (j - destPos)]));
            @*/
            for (int i = 0; i < length; i++) {
                dest[destPos + i] = src[srcPos + i];
            }
        } else {
            /*@
            loop_invariant -1 <= i && i < length;
            loop_invariant (\forall int j = srcPos .. srcPos + i + 1; src[j] == \old(src[j]));
            loop_invariant (\forall int j = destPos + i + 1 .. destPos + length; {:dest[j]:} == \old(src[srcPos + (j - destPos)]));
            @*/
            for (int i = length - 1; i >= 0; i--) {
                dest[destPos + i] = src[srcPos + i];
            }
        }
    }
}