class ClassWithJump {
  public static void main(java.lang.String[] args)
  {
    System.out.print(test(true));
  }

  private static int test(boolean b)
  {
    return b ? 3 : 4;
  }
}
