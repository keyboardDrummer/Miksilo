package transformations.bytecode.attributes

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, ParticleWithGrammar}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.readJar.ClassFileParser
import ClassFileParser._
trait ByteCodeAttribute extends ParticleWithGrammar {

  override def inject(state: CompilationState): Unit = {
    ByteCodeSkeleton.getState(state).attributes.put(constantPoolKey, this)
    super.inject(state)
  }

  def key: Key
  def getGrammar(grammars: GrammarCatalogue): BiGrammar
  def constantPoolKey: String

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val grammar = getGrammar(grammars)
    if (grammar == null) //TODO remove this silly hack.
      return

    val attributeGrammar = grammars.find(ByteCodeSkeleton.AttributeGrammar)
    attributeGrammar.addOption(grammar)
  }

  def getParser(unParsed: Node) : ClassFileParser.Parser[Node] = {
    val length = unParsed(UnParsedAttribute.UnParsedAttributeData).asInstanceOf[Seq[Byte]].length
    repN(length, ParseByte) ^^ (_ => unParsed.shallowClone)
  }
}
