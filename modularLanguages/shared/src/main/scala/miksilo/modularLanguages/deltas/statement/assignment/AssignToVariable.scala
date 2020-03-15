package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.StoreIntegerDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs.StoreLongDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects.StoreAddressDelta
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta.Shape
import miksilo.modularLanguages.deltas.bytecode.types.{ArrayTypeDelta, LongTypeDelta, QualifiedObjectTypeDelta, TypeSkeleton}
import miksilo.modularLanguages.deltas.expression.VariableDelta
import miksilo.modularLanguages.deltas.expression.VariableDelta.Variable
import miksilo.modularLanguages.deltas.javac.methods.{AssignmentToByteCodeDelta, VariableInfo}
import miksilo.modularLanguages.deltas.method.MethodDelta

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
