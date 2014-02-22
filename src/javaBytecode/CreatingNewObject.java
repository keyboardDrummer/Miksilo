package javaBytecode;

public class CreatingNewObject {
    int someField;
    public CreatingNewObject(int someInteger) {
        someField = someInteger + 3;
    }

    public CreatingNewObject(float someInteger) {
        this((int)someInteger + 3);
    }

    public static void blergh() {
        CreatingNewObject barp = new CreatingNewObject(4);
        int x = 10;
        new CreatingNewObject(2f);
    }
}
