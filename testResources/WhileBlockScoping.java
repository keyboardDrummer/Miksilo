class WhileBlockScoping {
    public static void main(String[] args) {

        int z;
        z = 3;
        while(z == 2)
        {
            int x;
            x = 3;
            continue;
        }

        while(z == 4)
        {
            int x;
            x = 2;
            continue;
        }

        System.out.print(1);
    }
}