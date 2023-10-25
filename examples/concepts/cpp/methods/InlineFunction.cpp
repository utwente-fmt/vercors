// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases InlineFunction
//:: tools silicon
//:: verdict Pass

//@ inline pure bool isPositive(int a) = (a >= 0);

//@ requires isPositive(number);
bool incr(int number);