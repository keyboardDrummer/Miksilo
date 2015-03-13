class IfElseBlockScoping {
    public static void main(String[] args) {
        if (3 == 3)
        {
            int x = 2;
            System.out.print(x);
        } else
        {
            int x = 4;
        }

    }
}