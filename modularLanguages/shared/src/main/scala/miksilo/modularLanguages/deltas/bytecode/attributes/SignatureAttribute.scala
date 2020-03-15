package miksilo.modularLanguages.deltas.bytecode.attributes

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.readJar.ClassFileParser
import miksilo.modularLanguages.deltas.bytecode.readJar.ClassFileParser._

object SignatureAttribute extends ByteCodeAttribute {

  override def description: String = "Adds the signature attribute"

  object SignatureKey extends NodeShape
  object SignatureIndex extends NodeField
  override def shape = SignatureKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    ("signatureIndex" ~ ":" ~~> integer.as(SignatureIndex)).asNode(SignatureKey)
  }

  override def constantPoolKey: String = "Signature"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = for {
    index <- ParseShort
  } yield new Node(SignatureKey, SignatureIndex -> index.toInt)

  override def getBytes(compilation: Compilation, node: Node): Seq[Byte] = ???
}
