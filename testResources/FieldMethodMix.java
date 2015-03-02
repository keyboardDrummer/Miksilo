
public class FieldMethodMix {
    int x;
    int y = z;
    int z = 3;
    int a = bla();

    int bla() {
        return a;
    }

    int blie() {
        return bla();
    }
}
