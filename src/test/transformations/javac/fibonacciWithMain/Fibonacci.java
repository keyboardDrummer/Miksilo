package transformations.javac.fibonacciWithMain;

class Fibonacci {
    public static void main(String[] args)
    {
        System.out.print(fibonacci(5));
    }

    public static int fibonacci(int index) {
        return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2);
    }
}