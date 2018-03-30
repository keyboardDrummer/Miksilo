package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.{KeyGrammar, LanguageGrammars}
import core.language.node.Node
import core.language.Language

trait ByteCodeTypeInstance extends TypeInstance {

  def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar

  def getStackType(_type: Node, state: Language): Node = _type

  def byteCodeGrammarKey = KeyGrammar(shape)
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    super.transformGrammars(grammars, language)
    val byteCodeGrammar = grammars.create(byteCodeGrammarKey, getByteCodeGrammar(grammars))
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addAlternative(byteCodeGrammar)
  }

  override def inject(language: Language): Unit = {
    super.inject(language)
    TypeSkeleton.byteCodeInstances.add(language, this)
  }
}
