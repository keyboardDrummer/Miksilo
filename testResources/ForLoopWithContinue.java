class ForLoopWithContinue
{
    public static void main(java.lang.String[] args)
    {
        int x = 0;
        for(int i = 0;i < 5;i++)
        {
            if (i == 1)
                continue;
            x += 2;

        }
        System.out.print(x);
    }
}