package transformations.bytecode.coreInstructions

import core.particles._
import core.particles.node.Node
import transformations.bytecode._
import transformations.bytecode.attributes.CodeAttribute.{InstructionSideEffectProvider, InstructionSignatureProvider, JumpBehavior}
import transformations.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.{ObjectTypeC, TypeSkeleton}

case class InstructionSignature(inputs: Seq[Node], outputs: Seq[Node])

class ByteCodeTypeException(message: String) extends Exception(message)

trait InstructionDelta extends InstructionWithGrammar with InstructionSignatureProvider with InstructionSideEffectProvider {

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    CodeAttribute.getInstructionSignatureRegistry(state).put(key, this)
    ByteCodeSkeleton.getState(state).getBytes.put(key, getInstructionByteCode)
    CodeAttribute.getInstructionSizeRegistry(state).put(key, getInstructionSize)
    CodeAttribute.getState(state).jumpBehaviorRegistry.put(key, jumpBehavior)
    CodeAttribute.getState(state).localUpdates.put(key, this)
  }

  def assertObjectTypeStackTop(stackTop: Node, name: String): Unit = {
    if (stackTop.clazz != ObjectTypeC.ObjectTypeKey)
      throw new ByteCodeTypeException(s"$name requires an object on top of the stack and not a $stackTop.")
  }

  def assertDoubleWord(state: CompilationState, input: Node): Unit = {
    if (TypeSkeleton.getTypeSize(input, state) != 2) {
      throw new ByteCodeTypeException("expected double word input")
    }
  }

  def assertSingleWord(state: CompilationState, input: Node): Unit = {
    if (TypeSkeleton.getTypeSize(input, state) != 1) {
      throw new ByteCodeTypeException("expected single word input")
    }
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)


  def getVariableUpdates(instruction: Node, typeState: ProgramTypeState): Map[Int, Node] = Map.empty
  def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature

  def jumpBehavior: JumpBehavior = new JumpBehavior(true, false)

  def getInstructionSize: Int = getInstructionByteCode(new Node(key, InstructionArgumentsKey -> List.range(0,10))).size
  def getInstructionByteCode(instruction: Node): Seq[Byte]


  protected def binary(_type: Node) = InstructionSignature(Seq(_type, _type), Seq(_type))

  override def description: String = s"Defines the $name instruction."
}
