class Fibonacci
{
    public static void main(java.lang.String[] args)
    {
        System.out.print(increment(1));
        System.out.print(increment(1l));
    }

    public static long increment(long value)
    {
        return 1l + value;
    }

    public static int increment(int value)
    {
        return 2 + value;
    }
}