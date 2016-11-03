class ComparisonOptimization
{
    public static void main(java.lang.String[] args)
    {
        canOptimize();
        cannotOptimize();
    }

    private static void cannotOptimize() {
        boolean x = 2 < 3;
        if (x)
            System.out.print(x);
    }

    private static void canOptimize() {
        if (2 < 3)
            System.out.print(5);
    }
}