package deltas.javac.expressions.postfix

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape, NodeField}
import core.deltas.path.Path
import core.deltas.{Compilation, Contract, Language}
import deltas.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta}
import deltas.bytecode.types.IntTypeC
import deltas.javac.expressions.{ExpressionInstance, ExpressionSkeleton}
import deltas.javac.methods.MethodDelta

object PostFixIncrementC extends ExpressionInstance {

  override val key = PostfixIncrementKey

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton, MethodDelta, IncrementIntegerDelta)

  override def getType(expression: Path, compilation: Compilation): Node = IntTypeC.intType

  override def toByteCode(plusPlus: Path, compilation: Compilation): Seq[Node] = {
    val methodCompiler = MethodDelta.getMethodCompiler(compilation)
    val name: String = plusPlus(VariableKey).asInstanceOf[String]
    val variableAddress = methodCompiler.getVariables(plusPlus)(name).offset
    Seq(LoadIntegerDelta.load(variableAddress), IncrementIntegerDelta.integerIncrement(variableAddress, 1))
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val coreGrammar = find(ExpressionSkeleton.CoreGrammar)
    val postFixIncrement = identifier.as(VariableKey) ~< "++" asNode PostfixIncrementKey
    coreGrammar.addOption(postFixIncrement)
  }

  object PostfixIncrementKey extends NodeShape

  object VariableKey extends NodeField

  override def description: String = "Adds the postfix ++ operator."
}
