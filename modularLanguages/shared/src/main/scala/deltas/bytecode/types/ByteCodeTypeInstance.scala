package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.{KeyGrammar, LanguageGrammars}
import core.language.Language
import core.language.node.Node
import core.smarts.types.objects.Type

trait ByteCodeTypeInstance extends TypeInstance {

  def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar

  def constraintName: Any
  def fromConstraintType(_type: Type): Node

  def getStackType(_type: Node, state: Language): Node = _type

  def byteCodeGrammarKey = KeyGrammar(shape)
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    super.transformGrammars(grammars, language)
    val byteCodeGrammar = grammars.create(byteCodeGrammarKey, getByteCodeGrammar(grammars))
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addAlternative(byteCodeGrammar)
  }

  def name: String

  override def inject(language: Language): Unit = {
    super.inject(language)
    TypeSkeleton.maps.put(constraintName, fromConstraintType)
    TypeSkeleton.byteCodeInstances.add(language, shape, this)
  }
}
