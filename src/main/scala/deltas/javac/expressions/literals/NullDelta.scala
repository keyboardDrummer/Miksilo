package deltas.javac.expressions.literals

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeShape}
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import deltas.bytecode.coreInstructions.objects.PushNullDelta
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object NullDelta extends ExpressionInstance {

  val _null = new Node(Shape)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val expressionGrammar = find(ExpressionSkeleton.ExpressionGrammar)
    val parseNull = "null" ~> value(_null)
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, PushNullDelta)

  object Shape extends NodeShape

  override val key = Shape

  override def getType(expression: NodePath, compilation: Compilation): Node = ???

  override def toByteCode(expression: NodePath, compilation: Compilation): Seq[Node] = {
    Seq(PushNullDelta.pushNull)
  }

  override def description: String = "Adds the usage of 'null'"
}
