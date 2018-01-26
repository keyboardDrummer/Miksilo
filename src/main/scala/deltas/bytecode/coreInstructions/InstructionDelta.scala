package deltas.bytecode.coreInstructions

import core.deltas._
import core.deltas.node.{Node, NodeLike, NodeShape, NodeWrapper}
import core.language.Language
import deltas.bytecode._
import deltas.bytecode.attributes.CodeAttributeDelta.{InstructionSideEffectProvider, InstructionSignatureProvider, JumpBehavior}
import deltas.bytecode.attributes.{CodeAttributeDelta, InstructionArgumentsKey}
import deltas.bytecode.coreInstructions.InstructionDelta.InstructionShape
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.{ObjectTypeDelta, TypeSkeleton}

case class InstructionSignature(inputs: Seq[Node], outputs: Seq[Node])

class ByteCodeTypeException(message: String) extends Exception(message)

object InstructionDelta {
  implicit class Instruction[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def delta: InstructionDelta = node.shape.asInstanceOf[InstructionShape].delta
    def jumpBehavior: JumpBehavior = delta.jumpBehavior
  }
  case class InstructionShape(delta: InstructionDelta) extends NodeShape {
    override lazy val toString: String = delta.name
  }
}

trait InstructionDelta extends InstructionWithGrammar
  with InstructionSignatureProvider with InstructionSideEffectProvider with NodeShape {

  final override val key = InstructionShape(this)

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.getRegistry(language).getBytes.put(key, getInstructionByteCode)
    CodeAttributeDelta.getRegistry(language).instructions.put(key, this)
  }

  def assertObjectTypeStackTop(stackTop: Node, name: String): Unit = {
    if (stackTop.shape != ObjectTypeDelta.ObjectTypeKey)
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

  def jumpBehavior: JumpBehavior = JumpBehavior(movesToNext = true, hasJumpInFirstArgument = false)

  def getInstructionSize: Int = getInstructionByteCode(new Node(key, InstructionArgumentsKey -> List.range(0,10))).size
  def getInstructionByteCode(instruction: Node): Seq[Byte]

  protected def binary(_type: Node) = InstructionSignature(Seq(_type, _type), Seq(_type))

  override def description: String = s"Defines the $name instruction."
}
