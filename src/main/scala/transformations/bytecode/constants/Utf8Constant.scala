package transformations.bytecode.constants
import core.bigrammar.BiGrammar
import core.grammar.StringLiteral
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import transformations.bytecode.ByteCodeSkeleton.identifier
import transformations.bytecode.PrintByteCode
import transformations.javac.classes.skeleton.QualifiedClassName

object Utf8Constant extends ConstantEntry {
  override def key = Utf8ConstantKey

  object Utf8ConstantKey extends Key
  object Value extends Key
  def create(value: String) = new Node(key, Value -> value)

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] =
    PrintByteCode.toUTF8ConstantEntry(constant(Value).asInstanceOf[String])

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    (identifier | StringLiteral : BiGrammar).asNode(Utf8ConstantKey, Value)

  override def description: String = "A string constant"
}

object QualifiedClassNameConstant extends ConstantEntry {
  override def key = QualifiedClassNameKey

  object QualifiedClassNameKey extends Key
  object Value extends Key
  def create(value: QualifiedClassName) = new Node(key, Value -> value)
  def get(node: Node): QualifiedClassName = node(Value).asInstanceOf[QualifiedClassName]

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] =
    PrintByteCode.toUTF8ConstantEntry(constant(Value).asInstanceOf[QualifiedClassName].parts.mkString("/"))

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    getQualifiedClassNameParser.asNode(key, Value)

  def getQualifiedClassNameParser: BiGrammar = {
    val construct: Any => Any = {
      case ids: Seq[Any] =>
        val stringIds = ids.collect({ case v: String => v})
        QualifiedClassName(stringIds)
    }
    val parseQualifiedClassName = identifier.someSeparated(".") ^^(construct, {
      case QualifiedClassName(stringIds) => Some(stringIds)
      case _ => None
    })
    parseQualifiedClassName
  }

  override def description: String = "A qualified class name constant"
}