Currently we use Scala's `object` to define keys for fields and for shapes. How does this compare to the alternatives? We could also simple create instances of a predefined NodeField and NodeShape class. However, then we wouldn't be able to use reflection to determine a nice name for the NodeField and NodeShape. We wouldn't either not have a name, which would worsen the debugging experience, or we would have to pass a name string, which would be quite a bit of duplication.

Using just literal strings as keys for fields as shapes does not seem ideal, since strings are guaranteed to be unique.

Is there a use to using Scala's dynamic type for Miksilo?

# Field objects
- Have intellisense
- Are guaranteed to be unique