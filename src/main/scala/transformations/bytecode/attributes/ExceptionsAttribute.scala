package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass}
import transformations.bytecode.readJar.ClassFileParser

object ExceptionsAttribute extends ByteCodeAttribute {

  object ExceptionsKey extends NodeClass
  override def key: Key = ExceptionsKey

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = ???

  override def constantPoolKey: String = "Exceptions"

  override def description: String = "Adds the exceptions attribute"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
