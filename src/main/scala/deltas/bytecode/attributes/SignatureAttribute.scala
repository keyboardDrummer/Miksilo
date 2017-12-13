package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Key, Node, NodeShape, NodeField}
import deltas.bytecode.readJar.ClassFileParser
import deltas.bytecode.readJar.ClassFileParser._

object SignatureAttribute extends ByteCodeAttribute {

  object SignatureKey extends NodeShape
  object SignatureIndex extends NodeField
  override def key: Key = SignatureKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    ("signature with index:" ~~> integer.as(SignatureIndex)).asNode(SignatureKey)
  }

  override def constantPoolKey: String = "Signature"

  override def description: String = "Adds the signature attribute"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = for {
    index <- ParseShort
  } yield new Node(SignatureKey, SignatureIndex -> index.toInt)
}
