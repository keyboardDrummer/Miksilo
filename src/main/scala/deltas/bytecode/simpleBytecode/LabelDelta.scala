package deltas.bytecode.simpleBytecode

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.StringLiteral
import core.deltas.Language
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import deltas.bytecode.attributes.StackMapTableAttribute.StackMapFrameGrammar
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}

object LabelDelta extends InstructionDelta {
  override val key = LabelKey

  object LabelKey extends NodeClass

  object Name extends NodeField

  object StackFrame extends NodeField

  implicit class Label[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def stackFrame: Node = node(StackFrame).asInstanceOf[Node]
    def name: String = node(Name).asInstanceOf[String]
  }

  def label(name: String, stackFrame: Node): Node = LabelKey.create(
    Name -> name,
    StackFrame -> stackFrame)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = throw new UnsupportedOperationException()

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    InstructionSignature(Seq.empty, Seq.empty)
  }

  override def getInstructionSize: Int = 0

  override def getGrammarForThisInstruction(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val stackMapFrameGrammar = find(StackMapFrameGrammar)
    grammarName ~~> StringLiteral.as(Name) %
      stackMapFrameGrammar.indent().as(StackFrame) asNode LabelKey
  }

  override def description: String = "Used to mark a specific point in an instruction list."

  override def grammarName: String = "label"
}
