package psp.std.api;

public enum PCmp {
  LT(-1), EQ(0), GT(1), NA(2);

  private int intValue;
  private PCmp(int intValue) { this.intValue = intValue; }
  public int intValue() { return intValue; }
  public PCmp flip() {
    if (intValue == -1) return GT;
    else if (intValue == 0) return EQ;
    else if (intValue == 1) return LT;

    return NA;
  }
}
