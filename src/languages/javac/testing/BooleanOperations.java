package languages.javac.testing;

public class BooleanOperations {
    public void main() {
        int three = 3;
        int four = 4;
        boolean lessThanResult = three < four;
        boolean andResult = lessThanResult && (4 == 5);
        if (andResult || false)
        {
            int berp = 0;
        }
        else
        {
            int barp = 1;
        }
    }
}
