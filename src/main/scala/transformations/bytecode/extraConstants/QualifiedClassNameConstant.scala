package transformations.bytecode.extraConstants

import core.bigrammar.BiGrammar
import core.particles.Language
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.constants.ConstantEntry
import transformations.javac.classes.skeleton.QualifiedClassName

/**
  * This is a virtual constant that translates into a UTF8Constant.
  */
object QualifiedClassNameConstant extends ConstantEntry {
  override def key = Key

  object Key extends NodeClass
  object Value extends NodeField

  def create(value: QualifiedClassName) = new Node(key, Value -> value)
  def get(node: Node): QualifiedClassName = node(Value).asInstanceOf[QualifiedClassName]

  override def getByteCode(constant: Node, state: Language): Seq[Byte] =
    PrintByteCode.toUTF8ConstantEntry(constant(Value).asInstanceOf[QualifiedClassName].parts.mkString("/"))

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    getQualifiedClassNameParser.asNode(key, Value)

  def getQualifiedClassNameParser: BiGrammar = {
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