package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, Key}
import deltas.bytecode.readJar.ClassFileParser

object InnerClassesAttribute extends ByteCodeAttribute {
  object InnerClasses
  override def key: Key = ???

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = ???

  override def constantPoolKey: String = "InnerClasses"

  override def description: String = "Adds the InnerClasses attribute."

    override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???
}
