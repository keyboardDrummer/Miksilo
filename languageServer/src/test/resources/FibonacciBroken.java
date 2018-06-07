class FibonacciBroken
{
    public static void main(java.lang.String[] args)
    {
        System.out.print(FibonacciBroken.fibonacci(5));
    }

    public static int fibonacci(int index)
    {
        return index < 2 ? 1 : FibonacciBroken.fibonacci(index2 - 1) + FibonacciBroken.fibonacci(index3 - 2);
    }
}