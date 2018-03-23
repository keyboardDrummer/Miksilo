package deltas.bytecode.extraConstants

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.constants.ConstantEntry
import deltas.javac.classes.skeleton.QualifiedClassName

/**
  * This is a virtual constant that translates into a UTF8Constant.
  */
object QualifiedClassNameConstantDelta extends ConstantEntry {
  override def shape = Key

  object Key extends NodeShape
  object Value extends NodeField

  implicit class QualifiedClassNameConstant[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def value: QualifiedClassName = node(Value).asInstanceOf[QualifiedClassName]
    def value_=(value: QualifiedClassName): Unit = node(Value) = value
  }

  def create(value: QualifiedClassName) = new Node(shape, Value -> value)
  def get(node: Node): QualifiedClassName = node(Value).asInstanceOf[QualifiedClassName]

  override def getBytes(compilation: Compilation, constant: Node): Seq[Byte] =
    PrintByteCode.toUTF8ConstantEntry(constant(Value).asInstanceOf[QualifiedClassName].parts.mkString("/"))

  override def getConstantEntryGrammar(grammars: LanguageGrammars): BiGrammar =
    getQualifiedClassNameParser(grammars).as(Value)

  def getQualifiedClassNameParser(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    identifier.someSeparated("/").map[Seq[String], QualifiedClassName](QualifiedClassName, qualifiedClassName => qualifiedClassName.parts)
  }

  override def description: String = "A qualified class name constant"

  override def getName = "Utf8" //TODO do I want this?
}