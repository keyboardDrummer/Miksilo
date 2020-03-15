package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.LoadIntegerDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs.LoadLongDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import miksilo.modularLanguages.deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, QualifiedObjectTypeDelta}
import miksilo.modularLanguages.deltas.expression.VariableDelta
import miksilo.modularLanguages.deltas.expression.VariableDelta._
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.javac.types.BooleanTypeDelta
import miksilo.modularLanguages.deltas.method.MethodDelta

object VariableToByteCodeDelta extends Delta with ConvertsToByteCodeDelta {

  override def description: String = "Compiles a variable to byte-code"

  override def dependencies: Set[Contract] = Set(VariableDelta, MethodDelta, LoadIntegerDelta)

  override val shape = Shape

  override def toByteCode(variable: NodePath, compilation: Compilation): Seq[Node] = {
    val variableInfo: VariableInfo = getVariableInfo(variable, compilation)
    val variableAddress = variableInfo.offset
    val _type = variableInfo._type
    Seq(_type.shape match {
      case BooleanTypeDelta.Shape => LoadIntegerDelta.load(variableAddress)
      case IntTypeDelta.Shape => LoadIntegerDelta.load(variableAddress)
      case LongTypeDelta.Shape => LoadLongDelta.load(variableAddress)
      case QualifiedObjectTypeDelta.Shape => LoadAddressDelta.addressLoad(variableAddress)
    })
  }
}
