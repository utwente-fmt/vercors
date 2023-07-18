// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Operators
//:: tools silicon
//:: verdict Pass

void test() {

    int num = 5;

    int numPlus = num + 5;
    //@ assert numPlus == 10;

    int numMinus = num - 5;
    //@ assert numMinus == 0;

    int numMult = num * 5;
    //@ assert numMult == 25;

    int numDiv = num / 5;
    //@ assert numDiv == 1;

    int numMod = num % 5;
    //@ assert numMod == 0;

    num++;
    //@ assert num == 6;

    num--;
    //@ assert num == 5;

    num += 5;
    //@ assert num == 10;

    num -= 5;
    //@ assert num == 5;

    num *= 5;
    //@ assert num == 25;

    num /= 5;
    //@ assert num == 5;

    num %= 5;
    //@ assert num == 0;

    int numMinus2 = -5;
    //@ assert numMinus2 == -5;

    int numPlus2 = +5;
    //@ assert numPlus2 == 5;

    bool boolean = true;

    bool booleanNot = !boolean;
    //@ assert booleanNot == false;

    bool booleanNot2 = not boolean;
    //@ assert booleanNot2 == false;

    bool booleanAnd = boolean && false;
    //@ assert booleanAnd == false;

    bool booleanOr = boolean || false;
    //@ assert booleanOr == true;

    bool booleanEquals = boolean == true;
    //@ assert booleanEquals == true;

    bool booleanNotEquals = boolean != true;
    //@ assert booleanNotEquals == false;

    num = 5;

    bool booleanLess = num < 6;
    //@ assert booleanLess == true;

    bool booleanLessEq = num <= 5;
    //@ assert booleanLessEq == true;

    bool booleanGreater = num > 4;
    //@ assert booleanGreater == true;

    bool booleanGreaterEq = num >= 5;
    //@ assert booleanGreaterEq == true;

    //@ assert 4 - 3 - 2 - 1 == (((4 - 3) - 2) - 1);

}