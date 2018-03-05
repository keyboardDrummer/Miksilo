package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.deltas.path.{NodePath, PathRoot, SequenceElement}
import core.language.{Compilation, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.MethodDelta

object WhileLoopDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Adds a while loop."

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(WhileKey, path => transformWhileLoop(path, compilation))
  }

  def transformWhileLoop(whileLoopPath: NodePath, compilation: Compilation): Unit = {
    val method = whileLoopPath.findAncestorShape(MethodDelta.Shape)
    val whileLoop: While[Node] = whileLoopPath.current
    val label: String = LabelDelta.getUniqueLabel("whileStart", method)
    val startLabel = JustJavaLabel.label(label)
    val ifBody = whileLoop.body ++ Seq(JustJavaGoto.goto(label))
    val _if = IfThenDelta.neww(whileLoop.condition, ifBody)

    val newStatements = Seq[Node](startLabel, _if)
    whileLoopPath.asInstanceOf[SequenceElement].replaceWith(newStatements)
  }

  override def dependencies: Set[Contract] = Set(IfThenDelta, BlockDelta, JavaGotoDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val expression = find(ExpressionSkeleton.ExpressionGrammar)
    val blockGrammar = find(BlockDelta.Grammar)
    val whileGrammar =
      "while" ~> expression.inParenthesis.as(Condition) %
      blockGrammar.as(Body) asLabelledNode WhileKey
    statementGrammar.addAlternative(whileGrammar)
  }

  implicit class While[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def condition: T = node(Condition).asInstanceOf[T]
    def body: Seq[T] = node(Body).asInstanceOf[Seq[T]]
  }

  def create(condition: Node, body: Seq[Node]) = new Node(WhileKey, Condition -> condition, Body -> body)

  object WhileKey extends NodeShape

  object Condition extends NodeField

  object Body extends NodeField
}
