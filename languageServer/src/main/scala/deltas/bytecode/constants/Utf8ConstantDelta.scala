package deltas.bytecode.constants

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.{Identifier, StringLiteral}
import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node._
import deltas.bytecode.PrintByteCode
import deltas.javac.classes.skeleton.QualifiedClassName

object Utf8ConstantDelta extends ConstantPoolEntry {
  override def shape = Utf8ConstantKey

  object Utf8ConstantKey extends NodeShape
  object Value extends NodeField
  def create(value: String) = new Node(shape, Value -> value)
  def get(constant: Node): String = constant(Value).asInstanceOf[String]

  implicit class Utf8Constant[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def value: String = node(Value).asInstanceOf[String]
    def value_=(value: String): Unit = node(Value) = value
  }

  def fromQualifiedClassName(name: QualifiedClassName): Node = create(name.parts.mkString("/"))
  def toQualifiedClassName(node: Node): QualifiedClassName = QualifiedClassName(node(Value).asInstanceOf[String].split("/"))

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] =
    PrintByteCode.toUTF8ConstantEntry(constant(Value).asInstanceOf[String])

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    regexGrammar("""[^\s,.]+""".r, "utf8 constant string").as(Value)
  }

  override def description: String = "A string constant"

  override val getName = "Utf8"
}
