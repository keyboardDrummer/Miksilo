package transformations.javac;

import java.util.HashSet;
import java.util.Set;

public class CallWithSubClass {

    void callsInterfaceTaker()
    {
        takesInterface(new HashSet<Integer>());
    }

    protected void takesInterface(Set someSet)
    {

    }
}
