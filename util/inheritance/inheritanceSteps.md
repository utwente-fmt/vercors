# Syntax elements

## Cast

```java
((Dog) animal)
```

### Safe mode

Should be converted into a function with `requires arg instanceof Dog`

### Unsafe mode

Should be converted into an abstract method with `ensures arg instanceof Dog` and `signals (ClassCastException e) !(e instanceof Dog)`

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

## Awareness of inherited fields

The resolver must traverse the type hierarchy to find fields declared in supertypes.

## Awareness of inherited methods

Idem, but for methods

## Shadowed fields

The resolver must be aware that duplicate field names are allowed between super & sub types. Also, the syntax for it, `super.x` and `Animal.x`

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

A downside to this approach is that transformations after the inheritance pass do not keep the `assume_Class` functions around, or mess them up somehow. I would say that's a plain bug. But we could also use the ADT encoding, as this would probably more robust against that kind of bug, in return for a slightly more complicated encoding, meaning more quantifiers.

## Assume exact dynamic type when constructing new object

The exact dynamic type should be added to the postcondition of constructors.

# Semantic checks to be generated

## Deriving static & dynamic contract from initial contract

Given a contract:

```
class Animal {
    int v1;
    resource state(int x) = Perm(v1, 1) ** v1 == x;

    context state(z);
    public abstract void sound(int z);
}
class Dog extends Animal {
    int v2;
    resource state(int x) = Perm(v2, 1) ** v2 == 2 * x;

    context state(z);
    @Override
    public void sound(int z) {
        // ...       
    }
}
```

The static contract for Dog.sound is:

```
context state@Dog(z);
```

The dynamic contract for Animal.sound is:

```
context state(z);
```

This is equal to the dynamic contract for Dog.sound. Bierman et al. have shown that this derivation holds.

## Method respects static contract

The method implementation has to obey the static contract.

## Static contract implies dynamic contract

The static contract and the dynamic contract + type info must be compatible. So the proof obligation for the method Dog.sound is:

```
requires this.class == Dog.class; // Exact type info known because of dynamic dispath
context state(z)
public void soundDynamic(int z) {
    unfold state(z) at Dog;
    //@ assert state@Dog(z);
    soundStaticContract(z);
    fold state(z) at Dog;
}
```

Where `soundStaticContract` is the generated method that has the static contract. This proof obligation ensures that, given the dynamic contract and type info of an object, the static contract of the corresponding method can safely be called into.

In practice, we do not have to generate the implementation of `soundDynamic`, since the correctness of this method with the static/dynamic contract derivation is gauranteed.

## Correct contract should be used

Private calls use static contract, method without qualifier (package-private) too. Rest should use dynamic contract. (Not 100% sure about package-private methods though)

Calls through `super` should also use static contract.

# Abstract predicate families

## Parent predicate is included

Each class-specific instance of an APF includes the APF of the supertype. So unfolding `state@Dog` also gives you `state@Animal`.

## Final predicates do not need dynamic type info to fold/unfold at

If the state predicate in Dog were marked final, no type information is needed to unfold the predicate if the static type is Dog, as it is guaranteed that the instance is state@Dog.

## Syntax for non-family predicates

If a predicate is marked as final it behaves as an ordinary predicate, that is, no type info is needed for unfolding. But that might be a bit counterintuitive. Maybe we need a "predicate" qualifier that ensures this behaviour? Might be easier to use and explain in the tutorial.

# New statements

## Extract statement

Given `state(z)`, executing `extract state(z) at Dog`, yields `state@Dog(z) ** (state@Dog(z) -* state(z))`. Useful for read only access of classes without knowing the dynamic type.

## unfold p() at C/fold p() at C

Given the dynamic type, exchange `state(z)` for `state@Dog(z)`.

## widen/narrow

Cool to have, but I think we can wait with implementing this until it comes up in a case study/standard lib. Adds/removes argument of an APF. E.g. `widen state(z) with int x` results in `state(z, x)`.
