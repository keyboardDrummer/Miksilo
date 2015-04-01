package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, Key}
import transformations.bytecode.readJar.ClassFileParser

object InnerClassesAttribute extends ByteCodeAttribute {
  object InnerClasses
  override def key: Key = ???

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = ???

  override def constantPoolKey: String = "InnerClasses"

  override def description: String = "Adds the InnerClasses attribute."

    override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
