class MultipleLocals {
  public static void multipleLocals(int param1, int param2, long param3) {
    int var1 = 0;
    while(var1 != 3) {
      int var2 = 3;
      long var3 = 4;
      var1++;
      System.console().printf("%i %i %l %i %i %l", param1, param2, param3, var1, var2, var3);
    }
  }
}