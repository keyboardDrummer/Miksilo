package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeField}
import core.particles.{Language, DeltaWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.readJar.ClassFileParser

object AttributeNameKey extends NodeField //TODO give this a proper place
trait ByteCodeAttribute extends DeltaWithGrammar {

  override def inject(state: Language): Unit = {
    ByteCodeSkeleton.getRegistry(state).attributes.put(constantPoolKey, this)
    super.inject(state)
  }

  def key: Key
  def getGrammar(grammars: GrammarCatalogue): BiGrammar
  def constantPoolKey: String

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val grammar = getGrammar(grammars)
    val attributeGrammar = grammars.find(ByteCodeSkeleton.AttributeGrammar)
    attributeGrammar.addOption(grammar)
  }

  def getParser(unParsed: Node) : ClassFileParser.Parser[Node]
}
