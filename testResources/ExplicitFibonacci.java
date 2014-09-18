import java.lang.*;
class Fibonacci
{
    public Fibonacci()
    {
        super();
        return;
    }

    public static void main(java.lang.String[] args)
    {
        System.out.print(fibonacci(5));
        return;
    }

    public static int fibonacci(int index)
    {
        return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2);
    }
}