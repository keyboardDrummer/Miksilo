ModularLanguage
=====
Miksilo allows defining languages in a modular way. Because languages share many properties, the quickest way to build a language is to build on top of existing ones. Here follows a number of features that simplify modular language construction.
  
### Delta's
Languages are defined by stacking many small language transformations called delta's onto an empty language. A single delta may add a feature like a while loop. Here is an example:
 
```scala
object WhileLoopDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Adds a while loop."

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._

    val statementGrammar = find(StatementDelta.Grammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val blockGrammar = find(BlockDelta.BlockGrammar)
    val whileGrammar = "while" ~> expression.inParenthesis.as(Condition) %
        blockGrammar.as(Body) asLabelledNode Shape
    statementGrammar.addAlternative(whileGrammar)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(Shape, path => transformWhileLoop(path, compilation))
  }

  def transformWhileLoop(whileLoopPath: NodePath, compilation: Compilation): Unit = {
    val whileLoop: While[Node] = whileLoopPath.current
    val label: String = LabelStatementDelta.getUniqueLabel(compilation, "whileStart", whileLoopPath)
    val startLabel = LabelStatementDelta.neww(label)
    val ifBody = BlockDelta.neww(Seq(whileLoop.body, GotoStatementDelta.neww(label)))
    val _if = IfThenDelta.neww(whileLoop.condition, ifBody)

    val newStatements = Seq[Node](startLabel, _if)
    whileLoopPath.asInstanceOf[NodeSequenceElement].replaceWith(newStatements)
  }

  override def dependencies: Set[Contract] = Set(IfThenDelta, BlockDelta, LabelStatementDelta, GotoStatementDelta)

  implicit class While[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def condition: T = node(Condition).asInstanceOf[T]
    def body: T = node(Body).asInstanceOf[T]
  }

  def create(condition: Node, body: Node) = new Node(Shape, Condition -> condition, Body -> body)

  object Shape extends NodeShape

  object Condition extends NodeField

  object Body extends NodeField
}
```
 
Delta's are packaged as reusable components with explicit dependencies between them. Delta may not only add to, but also remove from a language, which is a requirement for using deltas to transform between arbitrary languages.
 
Miksilo comes with a wide range of generic delta's that can be used to define common programming languages. This set includes deltas for adding expressions, statements and classes.

### Unstructed abstract syntax tree
The `Node` data-structure is used to store abstract syntax trees without structure, which allows efficiently changing the shape of the tree without the overhead of moving data into differently structured objects, which saves both developer and computational time. This also makes it easy to define tree transformations in a generic fashion.

The `Path` data-structure, which describes a path in the AST, can be used to easily traverse the tree, enabling traveling both 'up' and 'down' or replacing tree Nodes irrespective of their container.

### Metalanguages
Many libraries meant for constructing languages use 'metalanguages' to help the language developer specify different aspects of their language. Often these metalanguages are domain specific languages with no turing completeness.
 
Miksilo's uses metalanguages embedded in Scala. This has a cost in terms of the cleanliness of the syntax, although Scala has features that enable writing nice looking embedded languages. The advantage of embedding metalanguage inside a host language is that you stay inside the context of a turing complete language with a rich ecosystem, this allows traversing and manipulating metalanguage programs.

ModularLanguages's metalanguages are designed to be transformed. For example, early binding to fields in the grammar allows easy & safe editing.

### Examples
The best way to show Miksilo's modularity is by example, so we've picked a few delta's as showcases. Before diving into them however, we recommend to get [an introduction to BiGrammar](http://keyboarddrummer.github.io/Miksilo/grammar/introduction/), one of Miksilo's metalanguages. Here are the showcases:

1. [Add support for comments in a language agnostic way.](http://keyboarddrummer.github.io/Miksilo/grammar/trivia/)
1. [Inline the constant pool in Java bytecode.](http://keyboarddrummer.github.io/Miksilo/deltas/inline-constant-pool/)
1. [Resolving interactions between independent deltas.](http://keyboarddrummer.github.io/Miksilo/deltas/delta-interactions/)