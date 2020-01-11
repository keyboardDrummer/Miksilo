package deltas.statement.assignment

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.{Compilation, Language}
import core.language.node.Node
import deltas.bytecode.coreInstructions.integers.StoreIntegerDelta
import deltas.bytecode.coreInstructions.longs.StoreLongDelta
import deltas.bytecode.coreInstructions.objects.StoreAddressDelta
import deltas.bytecode.types.IntTypeDelta.Shape
import deltas.bytecode.types.{ArrayTypeDelta, LongTypeDelta, QualifiedObjectTypeDelta, TypeSkeleton}
import deltas.expression.VariableDelta
import deltas.expression.VariableDelta.Variable
import deltas.javac.methods.{AssignmentToByteCodeDelta, MethodDelta, VariableInfo}

object AssignToVariable extends DeltaWithGrammar {

  override def description: String = "Enables assigning to a variable."

  override def dependencies: Set[Contract] = Set(SimpleAssignmentDelta, VariableDelta)

  override def inject(language: Language): Unit = {
    AssignmentToByteCodeDelta.hasAssignFromStackByteCode.add(language, VariableDelta.Shape,
      (compilation: Compilation, _targetVariable: NodePath) => {
        val methodCompiler = MethodDelta.getMethodCompiler(compilation)
        val targetVariable: Variable[NodePath] = _targetVariable
        val variableInfo = methodCompiler.getVariables(targetVariable)(targetVariable.name)
        val byteCodeType = TypeSkeleton.toStackType(variableInfo._type, language)
        Seq(getStoreInstruction(variableInfo, byteCodeType))
      })
    super.inject(language)
  }

  def getStoreInstruction(variableInfo: VariableInfo, byteCodeType: Node): Node = {
    byteCodeType.shape match {
      case Shape => StoreIntegerDelta.integerStore(variableInfo.offset)
      case QualifiedObjectTypeDelta.StackType => StoreAddressDelta.addressStore(variableInfo.offset)
      case ArrayTypeDelta.Shape => StoreAddressDelta.addressStore(variableInfo.offset)
      case LongTypeDelta.Shape => StoreLongDelta.longStore(variableInfo.offset)
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val targetGrammar = grammars.find(SimpleAssignmentDelta.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableDelta.Shape)
    targetGrammar.addAlternative(variableGrammar)
  }
}
