class Whilee
{
    public static void main(String[] args)
    {
        int i = 0;
        while(i < 3)
        {
            i = i + 1;
        }
        while(i < 6)
        {
            i = i + 1;
            if(i==6){
                continue;
            }
            i = i + 1;
        }
        System.out.print(i);
    }
}