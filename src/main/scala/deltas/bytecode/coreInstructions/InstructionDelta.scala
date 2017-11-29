package deltas.bytecode.coreInstructions

import core.deltas._
import core.deltas.node.Node
import deltas.bytecode._
import deltas.bytecode.attributes.CodeAttribute.{InstructionSideEffectProvider, InstructionSignatureProvider, JumpBehavior}
import deltas.bytecode.attributes.{CodeAttribute, InstructionArgumentsKey}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.{ObjectTypeDelta, TypeSkeleton}

case class InstructionSignature(inputs: Seq[Node], outputs: Seq[Node])

class ByteCodeTypeException(message: String) extends Exception(message)

trait InstructionDelta extends InstructionWithGrammar
  with InstructionSignatureProvider with InstructionSideEffectProvider {

  override def inject(state: Language): Unit = {
    super.inject(state)
    CodeAttribute.getInstructionSignatureRegistry(state).put(key, this)
    ByteCodeSkeleton.getRegistry(state).getBytes.put(key, getInstructionByteCode)
    CodeAttribute.getInstructionSizeRegistry(state).put(key, getInstructionSize)
    CodeAttribute.getRegistry(state).jumpBehaviorRegistry.put(key, jumpBehavior)
    CodeAttribute.getRegistry(state).localUpdates.put(key, this)
  }

  def assertObjectTypeStackTop(stackTop: Node, name: String): Unit = {
    if (stackTop.clazz != ObjectTypeDelta.ObjectTypeKey)
      throw new ByteCodeTypeException(s"$name requires an object on top of the stack and not a $stackTop.")
  }

  def assertDoubleWord(language: Language, input: Node): Unit = {
    if (TypeSkeleton.getTypeSize(input, language) != 2) {
      throw new ByteCodeTypeException("expected double word input")
    }
  }

  def assertSingleWord(language: Language, input: Node): Unit = {
    if (TypeSkeleton.getTypeSize(input, language) != 1) {
      throw new ByteCodeTypeException("expected single word input")
    }
  }

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  def getVariableUpdates(instruction: Node, typeState: ProgramTypeState): Map[Int, Node] = Map.empty
  def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature

  def jumpBehavior: JumpBehavior = new JumpBehavior(true, false)

  def getInstructionSize: Int = getInstructionByteCode(new Node(key, InstructionArgumentsKey -> List.range(0,10))).size
  def getInstructionByteCode(instruction: Node): Seq[Byte]

  protected def binary(_type: Node) = InstructionSignature(Seq(_type, _type), Seq(_type))

  override def description: String = s"Defines the $name instruction."
}
