package deltas.javac.methods.assignment

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.Node
import core.deltas.path.NodePath
import core.language.Language
import deltas.bytecode.coreInstructions.integers.StoreIntegerDelta
import deltas.bytecode.coreInstructions.longs.StoreLongDelta
import deltas.bytecode.coreInstructions.objects.StoreAddressDelta
import deltas.bytecode.types.ArrayTypeDelta.ArrayTypeKey
import deltas.bytecode.types.IntTypeDelta.IntTypeKey
import deltas.bytecode.types.LongTypeDelta.LongTypeKey
import deltas.bytecode.types.{QualifiedObjectTypeDelta, TypeSkeleton}
import deltas.expressions.VariableDelta
import deltas.javac.methods.{MethodDelta, VariableInfo, VariableToByteCodeDelta}

object AssignToVariable extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(AssignmentSkeleton, VariableToByteCodeDelta)

  override def inject(language: Language): Unit = {
    AssignmentSkeleton.hasAssignFromStackByteCode.add(language, VariableDelta.Shape,
      (compilation, targetVariable: NodePath) => {
      val methodCompiler = MethodDelta.getMethodCompiler(compilation)
      val target = VariableDelta.getName(targetVariable)
      val variableInfo = methodCompiler.getVariables(targetVariable)(target)
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
    val variableGrammar = grammars.find(VariableDelta.VariableGrammar)
    targetGrammar.addAlternative(variableGrammar)
  }

  override def description: String = "Enables assigning to a variable."
}
