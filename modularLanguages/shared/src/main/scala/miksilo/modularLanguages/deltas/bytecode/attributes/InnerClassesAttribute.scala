package miksilo.modularLanguages.deltas.bytecode.attributes

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.readJar.ClassFileParser

object InnerClassesAttribute extends ByteCodeAttribute {

  override def shape = ???

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = ???

  override def constantPoolKey: String = "InnerClasses"

  override def description: String = "Adds the InnerClasses attribute."

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???

  override def getBytes(compilation: Compilation, node: Node): Seq[Byte] = ???
}
