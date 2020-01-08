class JavaGoto
{
    public static void main(java.lang.String[] args)
    {
        int x = 0;
        goto jo;
        label start;
        for(int i = 0;i < 5;i++)
        {
            x += 2;
        }
        goto end;
        label jo;
        x++;
        goto start;
        label end;
        System.out.print(x);
    }
}