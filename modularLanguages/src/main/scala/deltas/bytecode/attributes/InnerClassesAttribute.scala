package deltas.bytecode.attributes

import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.readJar.ClassFileParser

object InnerClassesAttribute extends ByteCodeAttribute {

  override def shape = ???

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = ???

  override def constantPoolKey: String = "InnerClasses"

  override def description: String = "Adds the InnerClasses attribute."

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???

  override def getBytes(compilation: Compilation, node: Node): Seq[Byte] = ???
}
