package transformations.javac.methods

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.LoadIntegerC
import transformations.javac.expressions.ExpressionC


object VariableC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(MethodC, LoadIntegerC)

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(VariableKey, (variable: MetaObject) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      val name: String = getVariableName(variable)
      val variableAddress = methodCompiler.variables(name).offset
      Seq(LoadIntegerC.integerLoad(variableAddress))
    })
    ExpressionC.getGetTypeRegistry(state).put(VariableKey, (variable: MetaObject) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      getType(variable, methodCompiler)
    })
  }

  def getVariableName(variable: MetaObject) = variable(VariableNameKey).asInstanceOf[String]

  def getType(variable: MetaObject, methodCompiler: MethodCompiler) = {
    methodCompiler.variables(VariableC.getVariableName(variable))._type
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val variableGrammar = identifier ^^ (name => variable(name.asInstanceOf[String]))
    expression.inner = expression.inner | variableGrammar
  }

  def variable(name: String) = new MetaObject(VariableKey, VariableNameKey -> name)

  object VariableNameKey

  object VariableKey

}
