package deltas.javac.methods

import core.deltas._
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import deltas.bytecode.coreInstructions.integers.LoadIntegerDelta
import deltas.bytecode.coreInstructions.longs.LoadLongDelta
import deltas.bytecode.coreInstructions.objects.LoadAddressDelta
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, QualifiedObjectTypeDelta}
import deltas.expressions.VariableDelta
import deltas.expressions.VariableDelta._
import deltas.javac.expressions.ConvertsToByteCode
import deltas.javac.types.BooleanTypeDelta

object VariableToByteCodeDelta extends Delta with ConvertsToByteCode {

  override def description: String = "Compiles a variable to byte-code"

  override def dependencies: Set[Contract] = Set(VariableDelta, MethodDelta, LoadIntegerDelta)

  override val shape = Shape

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
}
