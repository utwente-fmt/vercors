
//@ requires i;
void check(int i) {
  if(!i) {
    //@ assert false;
  }
}

void main() {
  check(5==5);
  int *p;
  if(!p) return;
}