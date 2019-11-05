procedure F(n: int) returns (r: int)
  ensures r < 90;     // Do you believe this one is true?
{
  if (100 < n) {
    r := 199 - n;
    return;
  } else {
    r := n - 10;
    //return;
  }
}

