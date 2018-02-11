package deltas.bytecode.coreInstructions.objects

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField}
import core.language.Language
import deltas.bytecode.constants.ClassInfoConstant
import deltas.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.QualifiedObjectTypeDelta
import deltas.bytecode.{ByteCodeSkeleton, PrintByteCode}

object NewByteCodeDelta extends InstructionDelta {
  
  def newInstruction(classRef: Any) = key.create(ClassRef -> classRef)
  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val location = instruction(ClassRef).asInstanceOf[Int]
    PrintByteCode.hexToBytes("bb") ++ PrintByteCode.shortToBytes(location)
  }
  override def getInstructionSize: Int = 3

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val classRef = instruction(ClassRef).asInstanceOf[Node]
    val className = QualifiedClassNameConstantDelta.get(classRef(ClassInfoConstant.Name).asInstanceOf[Node])
    val classType = QualifiedObjectTypeDelta.neww(className)
    InstructionSignature(Seq.empty, Seq(classType))
  }

  object ClassRef extends NodeField
  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(ClassRef -> ClassInfoConstant.key))
  }

  override def argumentsGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(ClassRef)
  }

  override def grammarName = "new"
}
