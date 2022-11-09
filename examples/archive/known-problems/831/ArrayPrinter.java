//:: case QuickSelectImpl
//:: tools silicon
//:: suite helper-file
// File is not included in testing: it is only there as a helper to execute a verified file.

package quickselect;

/**
* Helper class to print arrays for the QuickSelect example.
*/
public class ArrayPrinter {
    
    public static void printArray(int[] a, int l, int h){
        for(int i=0; i<a.length; i++){
            if(i==l || i==h+1) {
                System.out.print("|");
            } else {
                System.out.print(" ");
            }
            System.out.print(String.format("%2d", a[i]));
        }
        if(h==a.length-1) {
            System.out.print("|");
        }
        System.out.println();
    }
    
}