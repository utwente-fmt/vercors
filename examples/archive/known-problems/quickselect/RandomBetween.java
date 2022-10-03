//:: case QuickSelectImpl
//:: suite helper-file
// File is not included in testing: it is only there as a helper to execute a verified file.

package quickselect;

/**
* Implementation of random_between from the QuickSelect example.
*/
public class RandomBetween {
    
    //@ requires low<=high;
    //@ ensures low<=\result && \result<=high;
    public static int random_between(int low, int high) {
        return ((int)Math.random()*(high+1-low))+low;
    }
    
}