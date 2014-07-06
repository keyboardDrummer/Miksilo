class JavaScopes {
  public static void javaScopes() {
    int accum = 0;
    int var1 = 3;
    while(var1 < 4)
    {
      int var2 = 10;
      var1++;
      accum += var2;
    }

    while(var1 < 6) {
      int var3 = 15;
      var1++;
      accum += var3;
    }

    System.console().printf("%i", accum);
  }
}