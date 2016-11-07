package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, DeltaWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.readJar.ClassFileParser
trait ByteCodeAttribute extends DeltaWithGrammar {

  override def inject(state: CompilationState): Unit = {
    ByteCodeSkeleton.getState(state).attributes.put(constantPoolKey, this)
    super.inject(state)
  }

  def key: Key
  def getGrammar(grammars: GrammarCatalogue): BiGrammar
  def constantPoolKey: String

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val grammar = getGrammar(grammars)
    val attributeGrammar = grammars.find(ByteCodeSkeleton.AttributeGrammar)
    attributeGrammar.addOption(grammar)
  }

  def getParser(unParsed: Node) : ClassFileParser.Parser[Node]
}
