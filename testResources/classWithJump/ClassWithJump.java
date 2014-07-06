package classWithJump;

public class ClassWithJump {
  public static void main(String[] args)
  {
    System.out.print(test(true));
  }

  static int test(boolean b)
  {
    return b ? 3 : 4;
  }
}
