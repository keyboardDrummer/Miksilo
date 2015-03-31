package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode
import transformations.bytecode.constants.ConstantEntry

object CodeConstantEntry extends ConstantEntry //TODO deze kan toch helemaal weg??
{
  object CodeAttributeId

  def entry = new Node(CodeAttributeId)

  override def key: Any = CodeAttributeId

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    PrintByteCode.toUTF8ConstantEntry("Code")
  }

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = "Code" ~> produce(entry)

  override def description: String = "Adds a constant entry used by the code attribute to identity itself."
}
