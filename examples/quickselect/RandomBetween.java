//:: case QuickSelectImpl
//:: suite problem-fail
package quickselect;

/**
 * Implementation of random_between from the QuickSelect example.
 */
public class RandomBetween {

    //@ requires low<=high;
    //@ ensures low<=\result && \result<=high;
    public static int random_between(int low, int high) {
        return ((int) Math.random() * (high + 1 - low)) + low;
    }

}