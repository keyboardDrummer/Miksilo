package transformations.bytecode.constants

import core.bigrammar.{BiGrammar, FromGrammarWithToString}
import core.grammar.{Identifier, StringLiteral}
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass, NodeField}
import transformations.bytecode.PrintByteCode

object Utf8Constant extends ConstantEntry {
  override def key = Utf8ConstantKey

  object Utf8ConstantKey extends NodeClass
  object Value extends NodeField
  def create(value: String) = new Node(key, Value -> value)

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] =
    PrintByteCode.toUTF8ConstantEntry(constant(Value).asInstanceOf[String])

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar =
    (FromGrammarWithToString(Identifier, verifyWhenPrinting = true) |
      "<init>" | "<cinit>" | //TODO misschien een aparte constant maken voor 'Names'
      StringLiteral : BiGrammar).as(Value).asNode(Utf8ConstantKey)

  override def description: String = "A string constant"
}
