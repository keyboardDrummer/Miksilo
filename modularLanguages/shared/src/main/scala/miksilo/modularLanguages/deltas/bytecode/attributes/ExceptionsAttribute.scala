package miksilo.modularLanguages.deltas.bytecode.attributes

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.readJar.ClassFileParser

object ExceptionsAttribute extends ByteCodeAttribute {

  object Shape extends NodeShape
  override def shape = Shape

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = ???

  override def constantPoolKey: String = "Exceptions"

  override def description: String = "Adds the exceptions attribute"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ???

  override def getBytes(compilation: Compilation, node: Node): Seq[Byte] = ???
}
