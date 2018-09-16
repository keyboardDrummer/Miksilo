package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{ChildPath, NodePath}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.coreInstructions.integers.LoadIntegerDelta
import deltas.bytecode.coreInstructions.longs.LoadLongDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, QualifiedObjectTypeDelta}
import deltas.javac.expressions.{ConvertsToByteCode, ExpressionInstance}
import deltas.javac.types.BooleanTypeDelta
import deltas.expressions.VariableDelta._

object VariableToByteCodeDelta extends ExpressionInstance with ConvertsToByteCode {

  override def dependencies: Set[Contract] = Set(MethodDelta, LoadIntegerDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
  }

  override val shape = Shape

  override def getType(variable: NodePath, compilation: Compilation): Node = {
    getVariableInfo(variable, compilation)._type
  }

  def getVariableInfo(variable: NodePath, compilation: Compilation): VariableInfo = {
    MethodDelta.getMethodCompiler(compilation).getVariables(variable)(getName(variable))
  }

  override def toByteCode(variable: NodePath, compilation: Compilation): Seq[Node] = {
    val variableInfo: VariableInfo = getVariableInfo(variable, compilation)
    val variableAddress = variableInfo.offset
    val _type = variableInfo._type
    Seq(_type.shape match {
      case BooleanTypeDelta.BooleanTypeKey => LoadIntegerDelta.load(variableAddress)
      case IntTypeDelta.IntTypeKey => LoadIntegerDelta.load(variableAddress)
      case LongTypeDelta.LongTypeKey => LoadLongDelta.load(variableAddress)
      case QualifiedObjectTypeDelta.Shape => LoadAddressDelta.addressLoad(variableAddress)
    })
  }

  override def description: String = "Compiles a variable to byte-code"

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, variable: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.resolve(getName(variable), variable.asInstanceOf[ChildPath], parentScope, Some(_type))
  }
}
