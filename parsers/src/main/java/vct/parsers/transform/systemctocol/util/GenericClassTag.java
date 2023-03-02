package vct.parsers.transform.systemctocol.util;

import scala.Option;
import scala.collection.immutable.List;
import scala.collection.mutable.ArrayBuilder;
import scala.collection.mutable.ArraySeq;
import scala.reflect.ClassTag;
import scala.reflect.OptManifest;

public class GenericClassTag<T> implements ClassTag<T> {
    @Override
    public Class<?> erasure() {
        return ClassTag.super.erasure();
    }

    @Override
    public boolean $less$colon$less(ClassTag<?> that) {
        return ClassTag.super.$less$colon$less(that);
    }

    @Override
    public boolean $greater$colon$greater(ClassTag<?> that) {
        return ClassTag.super.$greater$colon$greater(that);
    }

    @Override
    public <A> Class<A[]> arrayClass(Class<?> tp) {
        return ClassTag.super.arrayClass(tp);
    }

    @Override
    public ClassTag<T[]> arrayManifest() {
        return ClassTag.super.arrayManifest();
    }

    @Override
    public T[][] newArray2(int len) {
        return ClassTag.super.newArray2(len);
    }

    @Override
    public T[][][] newArray3(int len) {
        return ClassTag.super.newArray3(len);
    }

    @Override
    public T[][][][] newArray4(int len) {
        return ClassTag.super.newArray4(len);
    }

    @Override
    public T[][][][][] newArray5(int len) {
        return ClassTag.super.newArray5(len);
    }

    @Override
    public ArraySeq<T> newWrappedArray(int len) {
        return ClassTag.super.newWrappedArray(len);
    }

    @Override
    public ArrayBuilder<T> newArrayBuilder() {
        return ClassTag.super.newArrayBuilder();
    }

    @Override
    public List<OptManifest<?>> typeArguments() {
        return ClassTag.super.typeArguments();
    }

    @Override
    public String argString() {
        return ClassTag.super.argString();
    }

    @Override
    public Class<?> runtimeClass() {
        return null;
    }

    @Override
    public ClassTag<T[]> wrap() {
        return ClassTag.super.wrap();
    }

    @Override
    public T[] newArray(int len) {
        return ClassTag.super.newArray(len);
    }

    @Override
    public Option<T> unapply(Object x) {
        return ClassTag.super.unapply(x);
    }

    @Override
    public boolean canEqual(Object x) {
        return ClassTag.super.canEqual(x);
    }
}
