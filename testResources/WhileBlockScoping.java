class WhileBlockScoping {
    public static void main(String[] args) {

        int z = 3;
        while(z == 2)
        {
            int x = 3;
        }

        while(z == 4)
        {
            int x = 2;
        }

        System.out.print(1);
    }
}