package transformations.javac.expressions.literals

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass}
import core.particles.path.Path
import transformations.bytecode.coreInstructions.objects.PushNullDelta
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}

object NullC extends ExpressionInstance {

  val _null = new Node(NullKey)

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val parseNull = "null" ~> produce(_null)
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, PushNullDelta)

  object NullKey extends NodeClass

  override val key = NullKey

  override def getType(expression: Path, state: Language): Node = ???

  override def toByteCode(expression: Path, state: Language): Seq[Node] = {
    Seq(PushNullDelta.pushNull)
  }

  override def description: String = "Adds the usage of 'null'"
}
