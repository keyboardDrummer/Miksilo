package deltas.bytecode.simpleBytecode

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.RegexGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.Language
import deltas.bytecode.attributes.StackMapTableAttribute.StackMapFrameGrammar
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}

import scala.collection.mutable

object LabelDelta extends InstructionDelta {

  def LabelKey = key //TODO inline

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

  def getNameGrammar: BiGrammar = RegexGrammar("""[\w<>\-]+""".r)
  override def getGrammarForThisInstruction(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val stackMapFrameGrammar = find(StackMapFrameGrammar)
    grammarName ~~> getNameGrammar.as(Name) %
      stackMapFrameGrammar.indent().as(StackFrame) asNode LabelKey
  }

  override def description: String = "Used to mark a specific point in an instruction list."

  override def grammarName: String = "label"

  object GeneratedLabels extends NodeField
  def getUniqueLabel(suggestion: String, methodInfo: Node): String = {
    val taken: mutable.Set[String] = methodInfo.data.getOrElseUpdate(GeneratedLabels, mutable.Set.empty).
      asInstanceOf[mutable.Set[String]]
    var result = suggestion
    var increment = 0
    while(taken.contains(result))
    {
      increment += 1
      result = suggestion + "_" + increment
    }
    taken.add(result)
    "<" + result + ">"
  }
}
