
public class FieldMethodMix {
    int x;
    int z = 3;
    int y = z;
    int a = bla();

    int bla() {
        return a;
    }

    int blie() {
        return bla();
    }

    int bloe() { return b; }

    int b = bloe();
}
