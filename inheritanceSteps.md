# Syntax elements

## Cast

```java
((Dog) animal)
```

### Safe mode

Should be converted into a function with `requires arg instanceof Dog`

### Unsafe mode

Should be converted into a method with `ensures arg instanceof Dog` and `signals (ClassCastException e) !(e instanceof Dog)`

## getClass

```java
javaObj.getClass()
```

Handled by contract on Object:

```java
package java.lang;

// Where do we put this? Maybe on the class "Class" as a static function?
/*@
 requires isJavaType(t);
 ensures \result != null;
 public pure Class TYPEToClass(TYPE t);
 @*/

public class Object {
    
    //@ ensures \result != null;
    // Candidate 1, but introduces forall:
    //@ ensures (\forall Object o; o != null && \typeof(this) == \typeof(o); o.getClass() == \result);
    // Candidat 2, without forall, but requires auxiliary definition. Uses a function's natural property that equal arguments means equal results
    //@ ensures \result == TYPEToClass(\typeof(this))
    final public /*@ pure @*/ Class getClass();
}
```

## Class literals

```java
Dog.class
```

Converts into

```java
TYPEToClass(TYPE.T_Dog())
```

## instanceof

```java
animal instanceof Dog
```

Converts into

```java
function javaInstanceOf(r: Ref, t: TYPE): Boolean {
    r != null && TYPE.issubtype(TYPE.typeof(r), t)
}

javaInstanceOf(animal, TYPE.T_Dog())
```

# Exceptions to check for

## ClassCastException

See the "Cast" section above

## ArrayStoreException

```java
xs[i] = dog;
```

For primitives, no check is needed.

For classes, converts into

### Safe mode

```java
assert TYPE.issubtype(TYPE.typeof(dog), TYPE.componentType(TYPE.typeof(xs)))
```

### Unsafe mode

```java
if (!TYPE.issubtype(dog, TYPE.componentType(TYPE.typeof(xs)))) {
    throw new ArrayStoreException();
}
```

# Java/COL Resolver extensions

## Shadowed fields

## Awareness of inherited fields

## Awareness of inherited methods

# Java type system encoding in COL/Silver

## Dynamic type subtype maintenance

Given:

```java
class Main {
    // Finds some main given an int
    //@ ensures \result != null;
    public static Main find(int i) {
        inhale false;
    }
    
    public static void main(String[] args) {
        Main m = Main.find(3);
        assert m instanceof Main;
    }
}
```

With a naive translation, turns into:

```java
// Finds some main given an int
//@ ensures \result != null;
Ref find(int i) {
    inhale false;
}

void main(String[] args) {
    Ref m = Main_find(3);
    //@ assert m != null && issubtype(typeof(m), TYPE.T_Main());
}
```

The assertion will fail, since return type information of `find` is lost.

Type information should be encoded in coercions to ref:

```
//@ ensures issubtype(typeof(r), TYPE.T_Main());
/*@ pure @*/ Ref assume_Main(Ref r) {
    return r;
}

// Finds some main given an int
//@ ensures \result != null;
Ref find(int i) {
    inhale false;
}

void main(String[] args) {
    Ref m = assume_Main(Main_find(3));
    assert m != null && issubtype(typeof(assume_Main(m)), TYPE.T_Main());
}
```

The assumptions are backed by the java type system.

Possibly, if a class is final, the function `assume_FinalClass` can emit more exact type info.

## Assume exact dynamic type when constructing new object

The exact dynamic type should be added to the postcondition of constructors.

# Semantic checks to be generated

## Method respects static contract

## Static contract implies dynamic contract

## Deriving static & dynamic contract from initial contract

## Correct contract should be used

Private calls use static contract, method without qualifier (package-private) too. Rest should use dynamic contract.

Calls through `super` should also use static contract.

# Abstract predicate families

## Parent predicate is included

## Final predicates do not need dynamic type info to fold/unfold at

## Syntax for non-family predicates

More or less sugar for adding `final` to a predicate def, but might be nice to have an easier to explain version of apfs.

# New statements

## Extract statement

## unfold p() at C/fold p() at C

## widen/narrow

Cool to have, but I think we can wait with implementing this until it comes up in a case study/standard lib