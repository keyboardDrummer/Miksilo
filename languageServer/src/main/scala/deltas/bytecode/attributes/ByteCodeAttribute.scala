package deltas.bytecode.attributes

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar, HasShape}
import core.language.Language
import core.language.node.{Node, NodeField, NodeShape}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.HasBytes
import deltas.bytecode.readJar.ClassFileParser

object AttributeNameKey extends NodeField //TODO give this a proper place
trait ByteCodeAttribute extends DeltaWithGrammar with HasShape with HasBytes {

  override def dependencies: Set[Contract] = Set[Contract](ByteCodeSkeleton)

  override def inject(language: Language): Unit = {
    ByteCodeSkeleton.attributesByName.get(language).put(constantPoolKey, this)
    ByteCodeSkeleton.hasBytes.add(language, this)
    super.inject(language)
  }

  def getGrammar(grammars: LanguageGrammars): BiGrammar
  def constantPoolKey: String

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val grammar = getGrammar(grammars)
    val attributeGrammar = grammars.find(ByteCodeSkeleton.AttributeGrammar)
    attributeGrammar.addAlternative(grammar)
  }

  def getParser(unParsed: Node) : ClassFileParser.Parser[Node]
}
