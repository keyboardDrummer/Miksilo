package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.node.{Node, NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.HasBytes
import miksilo.modularLanguages.deltas.bytecode._
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.{InstructionSideEffectProvider, InstructionSignatureProvider, JumpBehavior}
import miksilo.modularLanguages.deltas.bytecode.attributes.{CodeAttributeDelta, InstructionArgumentsKey}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.InstructionInstance.InstructionShape
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState
import miksilo.modularLanguages.deltas.bytecode.types.{QualifiedObjectTypeDelta, TypeSkeleton}

case class InstructionSignature(inputs: Seq[Node], outputs: Seq[Node])

class ByteCodeTypeException(message: String) extends Exception(message)

object InstructionInstance {
  implicit class Instruction[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def delta: InstructionInstance = node.shape.asInstanceOf[InstructionShape].delta
    def jumpBehavior: JumpBehavior = delta.jumpBehavior
  }
  case class InstructionShape(delta: InstructionInstance) extends NodeShape {
    override lazy val toString: String = delta.name
  }
}

trait InstructionInstance extends InstructionWithGrammar
  with InstructionSignatureProvider with InstructionSideEffectProvider with NodeShape with HasShape with HasBytes {

  final override val shape = InstructionShape(this)

  override def inject(language: Language): Unit = {
    super.inject(language)
    ByteCodeSkeleton.hasBytes.add(language, this)
    CodeAttributeDelta.instructions.add(language, this)
  }

  def assertObjectTypeStackTop(stackTop: Node, name: String): Unit = {
    if (stackTop.shape != QualifiedObjectTypeDelta.Shape)
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

  override def dependencies: Set[Contract] = super.dependencies ++ Set(ByteCodeSkeleton)

  def getVariableUpdates(instruction: Node, typeState: ProgramTypeState): Map[Int, Node] = Map.empty
  def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature

  def jumpBehavior: JumpBehavior = JumpBehavior(movesToNext = true, hasJumpInFirstArgument = false)

  def getInstructionSize(compilation: Compilation): Int = getBytes(compilation, new Node(shape, InstructionArgumentsKey -> List.range(0,10))).size

  protected def binary(_type: Node) = InstructionSignature(Seq(_type, _type), Seq(_type))

  override def description: String = s"Defines the $name instruction."
}
