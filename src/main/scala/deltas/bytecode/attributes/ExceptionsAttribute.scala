package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Key, Node, NodeClass}
import deltas.bytecode.readJar.ClassFileParser

object ExceptionsAttribute extends ByteCodeAttribute {

  object ExceptionsKey extends NodeClass
  override def key: Key = ExceptionsKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = ???

  override def constantPoolKey: String = "Exceptions"

  override def description: String = "Adds the exceptions attribute"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
