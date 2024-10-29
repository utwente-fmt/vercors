/*
This file models the ArrayList class from the Java standard library. It should verify in 10-20s with VerCors 2.0.

History:
- This file was initially created and verified by Joost Sessink using VerCors 1.4 for his capita selecta in 2022.
- Later in 2024 it was ported to VerCors 2.0 by Robert Rubbens, which required some syntactical changes but no additional verification effort. This version was then committed to the VerCors example folder.
*/

/*@
ghost
requires DummyArraysSupport.MAX_ARRAY_LENGTH >= 0;
context oldLength >= 0;
context minGrowth >= 0;
context prefGrowth >= 0;
ensures \result >= 0;
pure int ghostNewLength(int oldLength, int minGrowth, int prefGrowth) =
  (DummyMath.max(minGrowth, prefGrowth) + oldLength - DummyArraysSupport.MAX_ARRAY_LENGTH) <= 0 ?
    DummyMath.max(minGrowth, prefGrowth) + oldLength :
    ghostHugeLength(oldLength, minGrowth);

ghost
requires DummyArraysSupport.MAX_ARRAY_LENGTH >=0;
ensures \result >= 0;
ensures ((oldLength + minGrowth) <= DummyArraysSupport.MAX_ARRAY_LENGTH) ?
  \result == DummyArraysSupport.MAX_ARRAY_LENGTH :
  \result == 2147483647;
pure int ghostHugeLength(int oldLength, int minGrowth) =
  (oldLength + minGrowth) <= DummyArraysSupport.MAX_ARRAY_LENGTH ? DummyArraysSupport.MAX_ARRAY_LENGTH : 2147483647;
@*/

class TestArrayListObject {

  Object[] list;
  int size;
  private int DEFAULT_CAPACITY;
  private Object[] EMPTY_ELEMENTDATA;
  private Object[] DEFAULTCAPACITY_EMPTY_ELEMENTDATA;

  /*@
    requires initialCapacity >= 0;
  @*/
  public TestArrayListObject(int initialCapacity) throws Exception {
    this.DEFAULT_CAPACITY = 0;
    this.EMPTY_ELEMENTDATA = new Object[0];
    this.DEFAULTCAPACITY_EMPTY_ELEMENTDATA = new Object[0];
    if (initialCapacity > 0) {
      this.list = new Object[initialCapacity];
    } else if (initialCapacity == 0) {
      this.list = EMPTY_ELEMENTDATA;
    } else {
      throw new Exception();
    }
  }

  public TestArrayListObject() {
    this.DEFAULT_CAPACITY = 0;
    this.EMPTY_ELEMENTDATA = new Object[0];
    this.DEFAULTCAPACITY_EMPTY_ELEMENTDATA = new Object[0];
    this.list = DEFAULTCAPACITY_EMPTY_ELEMENTDATA;
  }

  /*@
    context Perm(size, 1);
    context Perm(list, 1);
    context Perm(DEFAULT_CAPACITY, 1\2);
    context Perm(DEFAULTCAPACITY_EMPTY_ELEMENTDATA, 1\2);
    context \array(list, list.length);
    context (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1));
    context size <= list.length && size >=0;
    context DummyArraysSupport.MAX_ARRAY_LENGTH >= 0;
    requires list.length >= 0 && list.length < DummyArraysSupport.MAX_ARRAY_LENGTH &&
      DummyArraysSupport.MAX_ARRAY_LENGTH <= 2147483647;
    requires value != null;
    ensures DEFAULT_CAPACITY == \old(DEFAULT_CAPACITY);
    ensures DEFAULTCAPACITY_EMPTY_ELEMENTDATA == \old(DEFAULTCAPACITY_EMPTY_ELEMENTDATA);
    ensures size == \old(size)+1;
    ensures (\old(list.length) == \old(size)) ?
    list.length > \old(list.length) :
    list.length == \old(list.length);
    ensures list[\old(size)] == value;
    ensures (\forall int j = 0 .. \old(size); (list[j] == \old(list[j])));
  @*/
  public boolean add(Object value) throws Exception {
    add(value, size);
    return true;
  }

  /*@
    context Perm(size, 1);
    context Perm(list, 1);
    context size >= 0;
    context list != null;
    requires s == size;
    requires list.length >= size;
    requires (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1));
    requires value != null;
    context Perm(DEFAULT_CAPACITY, 1\2);
    context Perm(DEFAULTCAPACITY_EMPTY_ELEMENTDATA, 1\2);
    context DummyArraysSupport.MAX_ARRAY_LENGTH >= 0;
    requires list.length >= 0 && list.length < DummyArraysSupport.MAX_ARRAY_LENGTH;
    requires DummyArraysSupport.MAX_ARRAY_LENGTH <= 2147483647;
    ensures DEFAULT_CAPACITY == \old(DEFAULT_CAPACITY);
    ensures DEFAULTCAPACITY_EMPTY_ELEMENTDATA == \old(DEFAULTCAPACITY_EMPTY_ELEMENTDATA);
    ensures (\forall* int b; b >= 0 && b < list.length; Perm(list[b], 1));
    ensures (s == \old(list.length)) ?
    list.length > \old(list.length) :
    list.length == \old(list.length);
    ensures size == \old(size)+1;
    ensures list.length >= size;
    ensures (\forall int j = 0 .. \old(size); ({: list[j] :} == \old(list[j])));
    ensures list[size-1] == value;
  @*/
  private void add(Object value, int s) throws Exception {
    if (s == list.length) {
      list = grow();
    }
    list[s] = value;
    size = s + 1;
  }

  /*@
    context Perm(list, 1);
    context Perm(size, 1\2);
    context list != null;
    requires (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1));
    requires (size + 1) > list.length;
    context Perm(DEFAULT_CAPACITY, 1\2);
    context Perm(DEFAULTCAPACITY_EMPTY_ELEMENTDATA, 1\2);
    context DummyArraysSupport.MAX_ARRAY_LENGTH >= 0;
    requires list.length >= 0 && list.length < DummyArraysSupport.MAX_ARRAY_LENGTH &&
    DummyArraysSupport.MAX_ARRAY_LENGTH <= 2147483647;
    ensures DEFAULT_CAPACITY == \old(DEFAULT_CAPACITY);
    ensures DEFAULTCAPACITY_EMPTY_ELEMENTDATA == \old(DEFAULTCAPACITY_EMPTY_ELEMENTDATA);
    ensures \result != null && \result.length > \old(list.length);
    ensures size == \old(size);
    ensures (\forall* int a; a >= 0 && a < \result.length; Perm(\result[a], 1));
    ensures (\forall int d = 0 .. \old(list.length); {: \result[d] :} == {: \old(list[d]) :});
  @*/
  private Object[] grow() throws Exception {
    return grow(size + 1);
  }

  /*@
    context Perm(list, 1);
    context Perm(size, 1\2);
    context list != null;
    requires \array(list, list.length);
    context Perm(DEFAULT_CAPACITY, 1\2);
    context Perm(DEFAULTCAPACITY_EMPTY_ELEMENTDATA, 1\2);
    context DummyArraysSupport.MAX_ARRAY_LENGTH >= 0;
    requires list.length >= 0 && list.length < DummyArraysSupport.MAX_ARRAY_LENGTH &&
    DummyArraysSupport.MAX_ARRAY_LENGTH <= 2147483647;
    requires minCapacity > list.length;
    requires (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1));
    ensures DEFAULT_CAPACITY == \old(DEFAULT_CAPACITY);
    ensures DEFAULTCAPACITY_EMPTY_ELEMENTDATA == \old(DEFAULTCAPACITY_EMPTY_ELEMENTDATA);
    ensures \result != null && \result.length > \old(list).length;
    ensures size == \old(size);
    ensures (\forall* int a; a >= 0 && a < \result.length; Perm(\result[a], 1));
    ensures (\forall int d = 0 .. \old(list.length); {: \result[d] :} == {: \old(list[d]) :});
  @*/
  private Object[] grow(int minCapacity) throws Exception {
    int oldCapacity = list.length;
    if (oldCapacity > 0 || list != DEFAULTCAPACITY_EMPTY_ELEMENTDATA) {
      int newCapacity =
          DummyArraysSupport.newLength(
              oldCapacity,
              minCapacity - oldCapacity, /* minimum growth */
              oldCapacity / 2 /* preferred growth */);
      list = DummyArrays.copyOf(list, newCapacity);
      return list;
    } else {
      list = new Object[DummyMath.max(DEFAULT_CAPACITY, minCapacity)];
      return list;
    }
  }

  /*@
    context_everywhere Perm(list, 1);
    context_everywhere Perm(size, 1);
    requires size >= 0 && size <= list.length;
    context_everywhere \array(list, list.length);
    context_everywhere (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1));
    requires (\forall int k = size .. list.length; list[k] == null);
    ensures size == 0;
    ensures list.length == \old(list.length);
    ensures(\forall int j = 0 .. list.length; list[j] == null);
  @*/
  public void clear() {
    final Object[] es = list;
    /*@
      loop_invariant list.length == \old(list.length);
      loop_invariant 0 <= i && i <= size;
      loop_invariant size >= 0 && size <= list.length;
      loop_invariant list.length >=0;
      loop_invariant size == \old(size);
      loop_invariant \array(es, list.length);
      loop_invariant es == list;
      loop_invariant (\forall int j = 0 .. i; list[j] == null);
      loop_invariant (\forall int l = 0 .. i; es[l] == null);
      loop_invariant (\forall int k = size .. list.length; list[k] == null);
      loop_invariant (\forall int m = size .. list.length; es[m] == null);
      loop_invariant (\forall int n = 0 .. list.length; es[n] == list[n]);
    @*/
    for (int i = 0; i < size; i++) {
      es[i] = null;
    }
    size = 0;
  }

  /*@
    context_everywhere Perm(list, 1);
    context_everywhere Perm(size, 1);
    context_everywhere \array(list, list.length);
    context_everywhere size <= list.length;
    requires index >= 0 && index < size;
    context_everywhere (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1));
    requires (\forall int k = size .. list.length; list[k] == null);
    ensures size == \old(size) - 1;
    ensures list.length == \old(list.length);
    ensures \result == \old(list[index]);
    ensures (\forall int j = 0 .. index; {: list[j] :} == \old( {: list[j] :} ));
    ensures (\forall int k = index .. list.length - 1; {: list[k] :} == \old(list[k+1]));
    ensures (list[list.length - 1] == null);
  @*/
  public Object remove(int index) throws Exception {
    if (index < 0 || index >= size) {
      throw new Exception();
    }
    Object removed_value = list[index];
    fastRemove(index);
    return removed_value;
  }

  /*@
    context_everywhere Perm(size, 1);
    context_everywhere Perm(list, 1);
    context_everywhere \array(list, list.length);
    context_everywhere size <= list.length;
    requires index >= 0 && index < size;
    context_everywhere (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1));
    requires (\forall int i = size .. list.length; list[i] == null);
    ensures size == \old(size) - 1;
    ensures list.length == \old(list.length);
    ensures (\forall int j = 0 .. index; {: list[j] :} == \old({: list[j] :}));
    ensures (\forall int k = index .. list.length - 1; {: list[k] :} == \old(list[k+1]));
    ensures (list[list.length-1] == null);
  @*/
  private void fastRemove(int index) {
    final int newSize = size - 1;
    if (newSize > index) {
      DummySystem.arraycopy(list, index + 1, list, index, newSize - index);
    }
    size = newSize;
    list[size] = null;
  }

  /*@
    context_everywhere Perm(list, 1\2);
    context_everywhere Perm(size, 1\2);
    context_everywhere \array(list, list.length);
    context_everywhere 0 <= size && size <= list.length;
    context_everywhere (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1\2));
    ensures size == \old(size);
    ensures list.length == \old(list.length);
    ensures list == \old(list);
    ensures (\forall int j = 0 .. size; (list[j] == \old(list[j])));
    ensures (\result ? (\exists int j = 0 .. size; (list[j] == value)) :
    (\forall int j = 0 .. size; (list[j] != value)));
  @*/
  public boolean contains(Object value) {
    return (indexOf(value) >= 0);
  }

  /*@
    context_everywhere Perm(size, 1\2);
    context_everywhere Perm(list, 1\2);
    context_everywhere \array(list, list.length);
    context_everywhere 0 <= size && size <= list.length;
    context_everywhere (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1\2));
    ensures size == \old(size);
    ensures list.length == \old(list.length);
    ensures list == \old(list);
    ensures (\forall int j = 0 .. size; (list[j] == \old(list[j])));
    ensures ((\result == -1) ?
    (\forall int j = 0 .. size; (list[j] != value)) :
    (\exists int j = 0 .. size; (list[j] == value)));
    ensures (-1 <= \result && \result <= list.length);
  @*/
  public int indexOf(Object value) {
    return indexOfRange(value, 0, size);
  }

  /*@
    context_everywhere Perm(size, 1\2);
    context_everywhere Perm(list, 1\2);
    context_everywhere \array(list, list.length);
    context_everywhere start >= 0 && start <= list.length;
    context_everywhere end >= start && end <= list.length;
    context_everywhere 0 <= size && size <= list.length;
    context_everywhere (\forall* int a; a >= 0 && a < list.length; Perm(list[a], 1\2));
    ensures size == \old(size);
    ensures list.length == \old(list.length);
    ensures list == \old(list);
    ensures (\forall int j = 0 .. size; (list[j] == \old(list[j])));
    ensures ((\result == -1) ?
    (\forall int j = start .. end; (list[j] != value)) :
    (\exists int j = start .. end; (list[j] == value)));
    ensures (-1 <= \result && \result <= list.length);
  @*/
  int indexOfRange(Object value, int start, int end) {
    Object[] es = list;
    if (value == null) {
      /*@
        loop_invariant list.length == \old(list.length);
        loop_invariant list == \old(list);
        loop_invariant size == \old(size);
        loop_invariant start <= i && i <= end && i >= 0 && i <= list.length ;
        loop_invariant \array(es, list.length);
        loop_invariant es == list;
        loop_invariant start >= 0 && end <= list.length;
        loop_invariant (\forall int k = start .. i; (list[k] != null));
        loop_invariant (\forall int j = start .. i; (es[j] != null));
        loop_invariant (\forall int l = 0 .. list.length; (es[l] == list[l]));
        loop_invariant (\forall int m = 0 .. list.length; (list[m] == \old(list[m])));
        loop_invariant (\forall int n = 0 .. list.length; (es[n] == \old(es[n])));
      @*/
      for (int i = start; i < end; i++) {
        if (es[i] == null) {
          return i;
        }
      }
    } else {
      /*@
        loop_invariant list.length == \old(list.length);
        loop_invariant list == \old(list);
        loop_invariant size == \old(size);
        loop_invariant start <= i && i <= end;
        loop_invariant \array(es, list.length);
        loop_invariant es == list;
        loop_invariant start >= 0 && end <= list.length;
        loop_invariant (\forall int k = start .. i; (list[k] != value));
        loop_invariant (\forall int j = start .. i; (es[j] != value));
        loop_invariant (\forall int l = 0 .. list.length; (es[l] == list[l]));
        loop_invariant (\forall int m = 0 .. list.length; (list[m] == \old(list[m])));
        loop_invariant (\forall int n = 0 .. list.length; (es[n] == \old(es[n])));
      @*/
      for (int i = start; i < end; i++) {
        if (es[i] == value) {
          return i;
        }
      }
    }
    return -1;
  }
}

class DummySystem {
  /*@
    context_everywhere length >= 0;
    context_everywhere \array(src, src.length);
    context_everywhere \array(dest, dest.length);
    requires 0 <= srcPos && srcPos + length <= src.length;
    requires 0 <= destPos && destPos + length <= dest.length;
    context_everywhere (\forall* int a; a >= 0 && a < dest.length; Perm(dest[a], 1));
    context_everywhere (dest != src) ==> (\forall* int b; b >= 0 && b < src.length; Perm(src[b], 1\2));
    ensures srcPos == \old(srcPos);
    ensures destPos == \old(destPos);
    ensures length == \old(length);
    ensures src.length == \old(src.length);
    ensures dest.length == \old(dest.length);
    ensures (\forall int h = 0 .. destPos; {: dest[h] :} == \old(dest[h]));
    ensures (\forall int i = destPos .. destPos + length; {: dest[i] :}  == \old(src[(i +srcPos - destPos)])) ; // The main clause that ensures the copying
    ensures (\forall int k = (destPos + length) .. dest.length; {: dest[k] :} == \old( {: dest[k] :} ));
    ensures (dest != src) ==> (\forall int l = 0 .. src.length; {: src[l] :} == \old(src[l]));
  @*/
  static void arraycopy(Object[] src, int srcPos, Object[] dest, int destPos, int length) {
    // @ assume false;
  }
}

class DummyArraysSupport {

  // private static int MAX_ARRAY_LENGTH; // = 2147483647 -8 == Integer.MAX_VALUE - 8;
  static final int MAX_ARRAY_LENGTH = 2147483647 - 8;

  /*@
    context MAX_ARRAY_LENGTH > oldLength && MAX_ARRAY_LENGTH <= 2147483647;
    ensures MAX_ARRAY_LENGTH == \old(MAX_ARRAY_LENGTH);
    context oldLength >= 0;
    context minGrowth > 0;
    context prefGrowth >=0;
    ensures \result > oldLength;
    ensures ((DummyMath.max(minGrowth, prefGrowth) + oldLength - MAX_ARRAY_LENGTH) <= 0) ?
    \result == (DummyMath.max(minGrowth, prefGrowth) + oldLength) :
    \result == ghostHugeLength(oldLength, minGrowth);
  @*/
  public static int newLength(int oldLength, int minGrowth, int prefGrowth) throws Exception {
    int newLength = DummyMath.max(minGrowth, prefGrowth) + oldLength;
    if (newLength - MAX_ARRAY_LENGTH <= 0) {
      return newLength;
    }
    return hugeLength(oldLength, minGrowth);
  }

  /*@
    signals (Exception e) (oldLength + minGrowth) < 0;
    requires oldLength >= 0;
    context MAX_ARRAY_LENGTH > oldLength && MAX_ARRAY_LENGTH <= 2147483647;
    ensures MAX_ARRAY_LENGTH == \old(MAX_ARRAY_LENGTH);
    ensures \result > oldLength;
    ensures ((oldLength + minGrowth) <= MAX_ARRAY_LENGTH) ?
    \result == MAX_ARRAY_LENGTH :
    \result == 2147483647;
  @*/
  private static int hugeLength(int oldLength, int minGrowth) throws Exception {
    int minLength = oldLength + minGrowth;
    if (minLength < 0) { // overflow
      throw new Exception();
    }
    if (minLength <= MAX_ARRAY_LENGTH) {
      return MAX_ARRAY_LENGTH;
    }
    return 2147483647; // Integer.MAX_VALUE;
  }
}

class DummyArrays {
  /*@
    requires newLength > 0;
    requires original != null;
    requires original.length >= 0;
    context (\forall* int a; a >= 0 && a < original.length; Perm(original[a], 1\2));
    ensures \array(\result, newLength);
    ensures (\forall* int i; i >=0 && i < \result.length; Perm(\result[i], 1));
    ensures (\forall int b = 0 .. DummyMath.min(original.length, newLength); {: original[b] :} == {: \result[b] :});
    ensures (\forall int c = 0 .. original.length; {: original[c] :} == \old(original[c]));
    ensures (newLength > original.length) ==>
    \result.length > original.length;
  @*/
  public static Object[] copyOf(Object[] original, int newLength) {
    Object[] copy = new Object[newLength];
    DummySystem.arraycopy(original, 0, copy, 0, DummyMath.min(original.length, newLength));
    return copy;
  }
}

class DummyMath {
  /*@
    ensures (a >= b) ? \result == a : \result == b;
    ensures (\result == a) ? a >= b : b > a;
  @*/
  public static /*@ pure @*/ int max(int a, int b) {
    return (a >= b) ? a : b;
  }

  /*@
    ensures (a <= b) ? \result == a : \result == b;
    ensures (\result == a) ? a <= b : b < a;
  @*/
  public static /*@ pure @*/ int min(int a, int b) {
    return (a <= b) ? a : b;
  }
}
