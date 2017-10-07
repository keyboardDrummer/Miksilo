package transformations.javac.expressions.postfix

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.path.Path
import core.particles.{Contract, Language}
import transformations.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta}
import transformations.bytecode.types.IntTypeC
import transformations.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import transformations.javac.methods.MethodC

object PostFixIncrementC extends ExpressionInstance {

  override val key = PostfixIncrementKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, MethodC, IncrementIntegerDelta)

  override def getType(expression: Path, state: Language): Node = IntTypeC.intType

  override def toByteCode(plusPlus: Path, state: Language): Seq[Node] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val name: String = plusPlus(VariableKey).asInstanceOf[String]
    val variableAddress = methodCompiler.getVariables(plusPlus)(name).offset
    Seq(LoadIntegerDelta.load(variableAddress), IncrementIntegerDelta.integerIncrement(variableAddress, 1))
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val coreGrammar = grammars.find(ExpressionSkeleton.CoreGrammar)
    val postFixIncrement = identifier.as(VariableKey) ~< "++" asNode PostfixIncrementKey
    coreGrammar.addOption(postFixIncrement)
  }

  object PostfixIncrementKey extends NodeClass

  object VariableKey extends NodeField

  override def description: String = "Adds the postfix ++ operator."
}
