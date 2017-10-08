package transformations.bytecode.coreInstructions.objects

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import core.particles.{Compilation, Language}
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionDelta, InstructionSignature}
import transformations.bytecode.extraConstants.QualifiedClassNameConstantDelta
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.ObjectTypeDelta
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}

object NewByteCodeDelta extends InstructionDelta {

  object NewByteCodeKey extends NodeClass
  
  def newInstruction(classRef: Any) = NewByteCodeKey.create(ClassRef -> classRef)
  
  override val key = NewByteCodeKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val location = instruction(ClassRef).asInstanceOf[Int]
    PrintByteCode.hexToBytes("bb") ++ PrintByteCode.shortToBytes(location)
  }
  override def getInstructionSize: Int = 3

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val classRef = instruction(ClassRef).asInstanceOf[Node]
    val className = QualifiedClassNameConstantDelta.get(classRef(ClassInfoConstant.Name).asInstanceOf[Node])
    val classType = ObjectTypeDelta.objectType(className)
    InstructionSignature(Seq.empty, Seq(classType))
  }

  object ClassRef extends NodeField
  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(ClassRef -> ClassInfoConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue) = grammars.find(ConstantPoolIndexGrammar).as(ClassRef)

  override def grammarName = "new"
}
