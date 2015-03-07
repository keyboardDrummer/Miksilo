
class FieldMethodMix {
    int x;
    int z = 3;
    int y = z;
    int a = bla();

    FieldMethodMix() {
    }

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
}
