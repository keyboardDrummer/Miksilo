package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Key

object ExceptionsAttribute extends ByteCodeAttribute {

  object ExceptionsKey extends Key
  override def key: Key = ExceptionsKey

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = null

  override def constantPoolKey: String = "Exceptions"

  override def description: String = "Adds the exceptions attribute"
}
