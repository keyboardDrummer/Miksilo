package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeField}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.constants.MethodRefConstant.MethodRefWrapper
import miksilo.modularLanguages.deltas.bytecode.constants._
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.{QualifiedObjectTypeDelta, TypeSkeleton}
import miksilo.modularLanguages.deltas.javac.types.MethodTypeDelta._

abstract class InvokeDelta extends InstructionInstance {

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val methodRef = getInvokeTargetMethodRef(instruction)
    val nameAndType = methodRef.nameAndType
    val descriptor = nameAndType._type.value
    getMethodStackModification(descriptor, language)
  }

  def getInstanceInstructionSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val methodRef = getInvokeTargetMethodRef(instruction)
    val nameAndType = methodRef.nameAndType
    val classRef = methodRef(MethodRefConstant.ClassRef).asInstanceOf[Node]
    val className = classRef(ClassInfoConstant.Name).asInstanceOf[Node]
    val classType = QualifiedObjectTypeDelta.neww(Utf8ConstantDelta.toQualifiedClassName(className))
    val descriptor = nameAndType._type.value
    val InstructionSignature(ins, outs) = getMethodStackModification(descriptor, language)
    InstructionSignature(Seq(classType) ++ ins, outs)
  }

  object MethodRef extends NodeField
  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(MethodRef -> MethodRefConstant.shape))
  }

  override def argumentsGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(MethodRef)
  }

  def getMethodStackModification(methodType: Node, language: Language): InstructionSignature = {
    val ins = methodType.parameterTypes.map(_type => TypeSkeleton.toStackType(_type, language))
    val outs = Seq(TypeSkeleton.toStackType(methodType.returnType, language))
    InstructionSignature(ins, outs.filter(p => TypeSkeleton.getTypeSize(p, language) > 0))
  }

  def getInvokeTargetMethodRef(instruction: Node): MethodRefWrapper[Node] = {
    instruction(MethodRef).asInstanceOf[Node]
  }

  override def getInstructionSize(compilation: Compilation): Int = 3
}
