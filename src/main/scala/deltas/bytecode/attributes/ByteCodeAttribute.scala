package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Key, Node, NodeField}
import core.deltas.{Language, DeltaWithGrammar}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.readJar.ClassFileParser

object AttributeNameKey extends NodeField //TODO give this a proper place
trait ByteCodeAttribute extends DeltaWithGrammar {

  override def inject(state: Language): Unit = {
    ByteCodeSkeleton.getRegistry(state).attributes.put(constantPoolKey, this)
    super.inject(state)
  }

  def key: Key
  def getGrammar(grammars: LanguageGrammars): BiGrammar
  def constantPoolKey: String

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val grammar = getGrammar(grammars)
    val attributeGrammar = grammars.find(ByteCodeSkeleton.AttributeGrammar)
    attributeGrammar.addOption(grammar)
  }

  def getParser(unParsed: Node) : ClassFileParser.Parser[Node]
}
