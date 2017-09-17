package transformations.bytecode.coreInstructions.objects

import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.constants.{ClassRefConstant, QualifiedClassNameConstant}
import transformations.bytecode.coreInstructions.{ConstantPoolIndexGrammar, InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.ObjectTypeC
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}

object NewByteCodeC extends InstructionC {

  object NewByteCodeKey extends NodeClass
  
  def newInstruction(classRef: Any) = NewByteCodeKey.create(ClassRef -> classRef)
  
  override val key: Key = NewByteCodeKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val location = instruction(ClassRef).asInstanceOf[Int]
    PrintByteCode.hexToBytes("bb") ++ PrintByteCode.shortToBytes(location)
  }
  override def getInstructionSize: Int = 3

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val constantPool = state.program.constantPool
    val location = instruction(ClassRef).asInstanceOf[Int]
    val classRef = constantPool.getValue(location).asInstanceOf[Node]
    val className = QualifiedClassNameConstant.get(constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[Node])
    val classType = ObjectTypeC.objectType(className)
    InstructionSignature(Seq.empty, Seq(classType))
  }

  object ClassRef extends NodeField
  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(ClassRef -> ClassRefConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue) = grammars.find(ConstantPoolIndexGrammar).as(ClassRef)
}
