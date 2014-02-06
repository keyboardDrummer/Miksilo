package javaInterpreter;

public class FibonacciExample {
  public static void main(String[] args) {
    fibonacci(5);
  }

  public static int fibonacci(int index) {
    return index < 2 ? 1 : fibonacci(index - 1) + fibonacci(index - 2);
  }
}
