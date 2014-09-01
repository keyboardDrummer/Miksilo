package transformations.javac.methods

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.coreInstructions.integers.LoadIntegerC
import transformations.bytecode.coreInstructions.longs.LoadLongC
import transformations.javac.expressions.ExpressionC
import transformations.types.{BooleanTypeC, LongTypeC, IntTypeC}


object VariableC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(MethodC, LoadIntegerC)

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(VariableKey, (variable: MetaObject) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      val name: String = getVariableName(variable)
      val variableInfo: VariableInfo = methodCompiler.variables(name)
      val variableAddress = variableInfo.offset
      val _type = variableInfo._type
      Seq(_type.clazz match {
        case BooleanTypeC.BooleanTypeKey => LoadIntegerC.load(variableAddress)
        case IntTypeC.IntTypeKey => LoadIntegerC.load(variableAddress)
        case LongTypeC.LongTypeKey => LoadLongC.load(variableAddress)
      })
    })
    ExpressionC.getGetTypeRegistry(state).put(VariableKey, (variable: MetaObject) => {
      val methodCompiler = MethodC.getMethodCompiler(state)
      getType(variable, methodCompiler)
    })
  }

  def getType(variable: MetaObject, methodCompiler: MethodCompiler) = {
    methodCompiler.variables(VariableC.getVariableName(variable))._type
  }

  def getVariableName(variable: MetaObject) = variable(VariableNameKey).asInstanceOf[String]

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val core = grammars.find(ExpressionC.CoreGrammar)
    val variableGrammar = grammars.create(VariableGrammar, identifier ^^ parseMap(VariableKey, VariableNameKey))
    core.inner = core.inner | variableGrammar
  }

  object VariableGrammar

  def variable(name: String) = new MetaObject(VariableKey, VariableNameKey -> name)

  object VariableNameKey

  object VariableKey

}
