//:: case QuickSelect
//:: tools silicon
package quickselect;

import static quickselect.RandomBetween.random_between;
import static quickselect.ArrayPrinter.printArray;

/**
* Quickselect is an algorithm to find the `k-th` smallest element, and it has an expected runtime of `O(n)`. It uses constant memory and in-place updates in the function 'partition', partially sorting the array during execution.
* Sequential version.
* @Author Henk Mulder.
*/
public class QuickSelect {

    //@ requires low<=high;
    //@ ensures low<=\result && \result<=high;
    static int random_between(int low, int high);
    
    //@ context a != null;
    //@ context Perm(a[*], 1\2);
    static void printArray(int[] a, int l, int h);

    //@ context a != null;
    //@ context Perm(a[l], write) ** Perm(a[h], write);
    //@ ensures a[l] == \old(a[h]);
    //@ ensures a[h] == \old(a[l]);
    public static void swap(int[] a, int l, int h) {
        int t = a[l];
        a[l] = a[h];
        a[h] = t;
    }

    //@ context_everywhere a != null;
    //@ context_everywhere 0<=low && low<=high && high<a.length;
    //@ context_everywhere (\forall* int i; 0<=i && i<a.length && (i<low || i>high); Perm(a[i], 1\2));
    //@ context_everywhere (\forall* int i; low<=i && i<=high; Perm(a[i], write));
    //@ requires (\exists int i; low<=i && i<=high; a[i] == pivot);
    //@ ensures low<=\result && \result<=high;
    //@ ensures (\forall int i; low<=i && i<\result; a[i] < pivot);
    //@ ensures (\forall int i; \result<=i && i<=high; pivot <= a[i]);
    //@ ensures a[\result] == pivot;
    //@ ensures (\forall int i; low<=i && i<\result; (\forall int j; \result<=j && j<=high; a[i] < a[j]));
    //@ ensures (\forall int i; low<=i && i<=\result; (\forall int j; \result<=j && j<=high; a[i] <= a[j]));
    //@ ensures (\forall int i; 0<=i && i<low; (\forall int j; low<=j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<low; (\forall int l; low<=l && l<a.length; a[k] <= a[l]));
    //@ ensures (\forall int i; 0<=i && i<=high; (\forall int j; high<j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<=high; (\forall int m; high<m && m<a.length; a[k] <= a[m]));
    public static int partition(int[] a, int low, int high, int pivot) {
        int l = low;
        int h = high;
        //@ loop_invariant low<=l && l<=high;
        //@ loop_invariant low<=h && h<=high;
        //@ loop_invariant (\forall int i; low<=i && i<l; a[i]<pivot);
        //@ loop_invariant (\forall int i; l<=i && h<i && i<=high; pivot <= a[i]);
        //@ loop_invariant l<=h;
        //@ loop_invariant (\exists int i; l<=i && i<=h; a[i] == pivot);
        //@ loop_invariant (\forall int i; 0<=i && i<low; (\forall int j; low<=j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<low; (\forall int m; low<=m && m<a.length; a[k] <= a[m]));
        //@ loop_invariant (\forall int i; 0<=i && i<=high; (\forall int j; high<j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<=high; (\forall int m; high<m && m<a.length; a[k] <= a[m]));
        while(true){
            //@ loop_invariant low<=l && l<=high;
            //@ loop_invariant low<=h && h<=high;
            //@ loop_invariant (\forall int i; low<=i && i<l; a[i]<pivot);
            //@ loop_invariant (\forall int i; l<=i && h<i && i<=high; pivot <= a[i]);
            //@ loop_invariant l<=h;
            //@ loop_invariant (\exists int i; l<=i && i<=h; a[i] == pivot);
            //@ loop_invariant (\forall int i; 0<=i && i<low; (\forall int j; low<=j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<low; (\forall int m; low<=m && m<a.length; a[k] <= a[m]));
            //@ loop_invariant (\forall int i; 0<=i && i<=high; (\forall int j; high<j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<=high; (\forall int m; high<m && m<a.length; a[k] <= a[m]));
            while(a[l] < pivot && l<h){
                l = l+1;
            }
            //@ loop_invariant low<=l && l<=high;
            //@ loop_invariant a[l]>=pivot || l>=h;
            //@ loop_invariant low<=h && h<=high;
            //@ loop_invariant (\forall int i; low<=i && i<l; a[i]<pivot);
            //@ loop_invariant (\forall int i; l<=i && h<i && i<=high; pivot <= a[i]);
            //@ loop_invariant l<=h;
            //@ loop_invariant a[l] >= pivot;
            //@ loop_invariant (\exists int i; l<=i && i<=h; a[i] == pivot);
            //@ loop_invariant (\forall int i; 0<=i && i<low; (\forall int j; low<=j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<low; (\forall int m; low<=m && m<a.length; a[k] <= a[m]));
            //@ loop_invariant (\forall int i; 0<=i && i<=high; (\forall int j; high<j && j<a.length; \old(a[i]) <= \old(a[j]))) ==> (\forall int k; 0<=k && k<=high; (\forall int m; high<m && m<a.length; a[k] <= a[m]));
            while(a[h] > pivot && h>l){
                h = h-1;
            }
            if(l>=h) {
                return l;
            }else{
                swap(a, l, h);
                if(a[l] < pivot) {
                    l = l+1;
                } else {
                    h = h-1;
                }
            }
        }
    }

    //@ context_everywhere list != null;
    //@ context_everywhere list.length > 0;
    //@ context  0<=k && k<list.length;
    //@ context_everywhere (\forall* int i; 0<=i && i<list.length; Perm(list[i], write));
    //@ ensures (\exists int i; 0<=i && i<list.length; list[i] == \result);
    //@ ensures (\forall int i; 0<=i && i<=k; list[i] <= \result);
    //@ ensures (\forall int i; k<= i && i<list.length; \result <= list[i]);
    public static int select(int[] list, int k){
        int left = 0;
        int right = list.length-1;
        int pivotIndex = k;
        int pivot = list[pivotIndex];
        //@ loop_invariant 0<=left && left<list.length;
        //@ loop_invariant 0<=right && right<list.length;
        //@ loop_invariant left<=pivotIndex && pivotIndex<=right;
        //@ loop_invariant left <= k && right >= k;
        //@ loop_invariant (\forall int i; 0<=i && i<left; (\forall int j; left<=j && j<list.length; list[i] <= list[j]));
        //@ loop_invariant (\forall int i; 0<=i && i<=right; (\forall int j; right<j && j<list.length; list[i] <= list[j]));
        while (left < right) {
            pivot = list[pivotIndex];
            pivotIndex = partition(list, left, right, pivot);
            if (k == pivotIndex) {
                return list[k];
            }           else  {
                if (k < pivotIndex) {
                    right = pivotIndex - 1;
                }else{
                    left = pivotIndex + 1;
                }
            }
            printArray(list, left, right);
            pivotIndex = random_between(left, right);
        }
        return list[left];
    } 
    
    //@ requires false;
    public static void main(String[] args) {
        int[] nums = new int[20];
        for(int i=0; i<nums.length; i++){
            nums[i] = (int)(100*Math.random());
        }
        int k = (int)(nums.length*Math.random());
        String ks = "th";
        if(k==0) ks = "st";
        if(k==1) ks = "nd";
        System.out.println(String.format("Find the %d%s smallest number in", k+1, ks));
        printArray(nums, 0, nums.length-1);
        System.out.println("...");
        int kth = select(nums, k);
        printArray(nums, k, k);
    }
    
}
