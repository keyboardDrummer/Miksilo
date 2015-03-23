
class FieldMethodMix {
    int x;
    int z = 3;
    int y = z;
    int a = bla();

    FieldMethodMix(int c) {
        b = c;
    }

    int bla() {
        return a;
    }

    int blie() {
        return bla();
    }

    int bloe() { return b; }

    int b = bloe();

    public static void main(String[] args) {
        new FieldMethodMix(5).print();
    }

    void print() {
        System.out.print(x + z + y + a + b);
    }
}
