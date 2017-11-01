package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeClass, NodeField}
import transformations.bytecode.readJar.ClassFileParser
import transformations.bytecode.readJar.ClassFileParser._

object SignatureAttribute extends ByteCodeAttribute {

  object SignatureKey extends NodeClass
  object SignatureIndex extends NodeField
  override def key: Key = SignatureKey

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    import grammars._
    ("signature with index:" ~~> integer.as(SignatureIndex)).asNode(SignatureKey)
  }

  override def constantPoolKey: String = "Signature"

  override def description: String = "Adds the signature attribute"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = for {
    index <- ParseShort
  } yield new Node(SignatureKey, SignatureIndex -> index.toInt)
}
