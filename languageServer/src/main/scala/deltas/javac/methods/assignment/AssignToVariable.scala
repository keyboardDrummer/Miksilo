package deltas.javac.methods.assignment

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.Language
import core.language.node.Node
import deltas.bytecode.coreInstructions.integers.StoreIntegerDelta
import deltas.bytecode.coreInstructions.longs.StoreLongDelta
import deltas.bytecode.coreInstructions.objects.StoreAddressDelta
import deltas.bytecode.types.ArrayTypeDelta.ArrayTypeKey
import deltas.bytecode.types.IntTypeDelta.IntTypeKey
import deltas.bytecode.types.LongTypeDelta.LongTypeKey
import deltas.bytecode.types.{QualifiedObjectTypeDelta, TypeSkeleton}
import deltas.expressions.VariableDelta
import deltas.expressions.VariableDelta.Variable
import deltas.javac.methods.{MethodDelta, VariableInfo}

object AssignToVariable extends DeltaWithGrammar {

  override def description: String = "Enables assigning to a variable."

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, VariableDelta)

  override def inject(language: Language): Unit = {
    AssignmentSkeleton.hasAssignFromStackByteCode.add(language, VariableDelta.Shape,
      (compilation, _targetVariable: NodePath) => {
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
      case IntTypeKey => StoreIntegerDelta.integerStore(variableInfo.offset)
      case QualifiedObjectTypeDelta.StackType => StoreAddressDelta.addressStore(variableInfo.offset)
      case ArrayTypeKey => StoreAddressDelta.addressStore(variableInfo.offset)
      case LongTypeKey => StoreLongDelta.longStore(variableInfo.offset)
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val targetGrammar = grammars.find(AssignmentSkeleton.AssignmentTargetGrammar)
    val variableGrammar = grammars.find(VariableDelta.Shape)
    targetGrammar.addAlternative(variableGrammar)
  }
}
