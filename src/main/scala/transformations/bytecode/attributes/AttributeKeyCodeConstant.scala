package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeClass}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.constants.ConstantEntry

trait AttributeKeyCodeConstant extends ConstantEntry {

  object Key extends NodeClass

  def entry = new Node(key)

  override def key = Key

  def name: String

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    PrintByteCode.toUTF8ConstantEntry(name)
  }

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = name ~> produce(entry)

  override def description: String = s"Adds a constant entry used by the $name attribute to identity itself."
}
