package transformations.bytecode.extraConstants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node._
import transformations.bytecode.PrintByteCode
import transformations.bytecode.constants.ConstantEntry
import transformations.javac.classes.skeleton.QualifiedClassName

/**
  * This is a virtual constant that translates into a UTF8Constant.
  */
object QualifiedClassNameConstantDelta extends ConstantEntry {
  override def key = Key

  object Key extends NodeClass
  object Value extends NodeField

  implicit class QualifiedClassNameConstant[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def value: QualifiedClassName = node(Value).asInstanceOf[QualifiedClassName]
    def value_=(value: QualifiedClassName): Unit = node(Value) = value
  }

  def create(value: QualifiedClassName) = new Node(key, Value -> value)
  def get(node: Node): QualifiedClassName = node(Value).asInstanceOf[QualifiedClassName]

  override def getByteCode(constant: Node, state: Language): Seq[Byte] =
    PrintByteCode.toUTF8ConstantEntry(constant(Value).asInstanceOf[QualifiedClassName].parts.mkString("/"))

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    getQualifiedClassNameParser(grammars).as(Value)

  def getQualifiedClassNameParser(grammars: GrammarCatalogue): BiGrammar = {
    import grammars._
    val construct: Any => Any = {
      case ids: Seq[Any] =>
        val stringIds = ids.collect({ case v: String => v})
        QualifiedClassName(stringIds)
    }
    val parseQualifiedClassName = identifier.someSeparated("/") ^^ (construct, {
      case QualifiedClassName(stringIds) => Some(stringIds)
      case _ => None
    })
    parseQualifiedClassName
  }

  override def description: String = "A qualified class name constant"

  override def getName = "Utf8" //TODO do I want this?
}