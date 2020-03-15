package miksilo.modularLanguages.deltas.bytecode.coreInstructions.objects

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeField}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.constants.{ClassInfoConstant, Utf8ConstantDelta}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionInstance, InstructionSignature}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.QualifiedObjectTypeDelta
import miksilo.modularLanguages.deltas.bytecode.{ByteCodeSkeleton, PrintByteCode}

object NewByteCodeDelta extends InstructionInstance {
  
  def newInstruction(classRef: Any) = shape.create(ClassRef -> classRef)
  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val location = instruction(ClassRef).asInstanceOf[Int]
    PrintByteCode.hexToBytes("bb") ++ PrintByteCode.shortToBytes(location)
  }
  override def getInstructionSize(compilation: Compilation): Int = 3

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val classRef = instruction(ClassRef).asInstanceOf[Node]
    val className = Utf8ConstantDelta.toQualifiedClassName(classRef(ClassInfoConstant.Name).asInstanceOf[Node])
    val classType = QualifiedObjectTypeDelta.neww(className)
    InstructionSignature(Seq.empty, Seq(classType))
  }

  object ClassRef extends NodeField
  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.constantReferences.add(language, shape, Map(ClassRef -> ClassInfoConstant.shape))
  }

  override def argumentsGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(ClassRef)
  }

  override def grammarName = "new"
}
