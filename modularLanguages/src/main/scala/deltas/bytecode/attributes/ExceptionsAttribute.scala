package deltas.bytecode.attributes

import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.{Node, NodeShape}
import deltas.bytecode.readJar.ClassFileParser

object ExceptionsAttribute extends ByteCodeAttribute {

  object Shape extends NodeShape
  override def shape = Shape

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = ???

  override def constantPoolKey: String = "Exceptions"

  override def description: String = "Adds the exceptions attribute"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???

  override def getBytes(compilation: Compilation, node: Node): Seq[Byte] = ???
}
