class Fibonacci
{
    int fieldWithInitialiser = 3;

    public static void main(java.lang.String[] args)
    {
        System.out.println(new Fibonacci().fibonacci(5));
        runForLoop();
    }

    static void runForLoop()
    {
        int x = 0;
        for(int i = 0;i < 5;i++)
        {
            x += 2;
        }
        System.out.println(x);
    }

    int fibonacci(int index)
    {
        return index < 2 ? 1 : fibonacci(index - 1) + fibonacci(index - 2);
    }
}