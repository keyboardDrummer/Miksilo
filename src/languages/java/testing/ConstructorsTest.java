package languages.java.testing;

public class ConstructorsTest {
    public ConstructorsTest(int x) {

    }

    private ConstructorsTest(String b) {

    }

    public ConstructorsTest(Float y) {
        this(y.toString());
    }
}
