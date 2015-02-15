class Fibonacci
{
    public static void main(String[] args)
    {
        System.out.print(new Fibonacci().fibonacci(5));
    }

    public int fibonacci(int index)
    {
        return index < 2 ? 1 : fibonacci(index - 1) + fibonacci(index - 2);
    }
}