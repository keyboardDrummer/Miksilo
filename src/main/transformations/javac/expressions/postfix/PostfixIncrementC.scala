package transformations.javac.expressions.postfix

import core.transformation.grammars.GrammarCatalogue
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.coreInstructions.integers.{IncrementIntegerC, LoadIntegerC}
import transformations.javac.expressions.{ExpressionC, ExpressionInstance}
import transformations.javac.methods.MethodC
import transformations.types.IntTypeC

object PostFixIncrementC extends ExpressionInstance {

  override val key: AnyRef = PostfixIncrementKey

  override def dependencies: Set[Contract] = Set(ExpressionC, MethodC, IncrementIntegerC)

  override def getType(expression: MetaObject, state: TransformationState): MetaObject = IntTypeC.intType

  override def toByteCode(plusPlus: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val methodCompiler = MethodC.getMethodCompiler(state)
    val name: String = plusPlus(VariableKey).asInstanceOf[String]
    val variableAddress = methodCompiler.variables(name).offset
    Seq(LoadIntegerC.load(variableAddress), IncrementIntegerC.integerIncrement(variableAddress, 1))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val coreGrammar = grammars.find(ExpressionC.CoreGrammar)
    val postFixIncrement = identifier <~ "++" ^^ parseMap(PostfixIncrementKey, VariableKey)
    coreGrammar.addOption(postFixIncrement)
  }

  object PostfixIncrementKey

  object VariableKey
}
