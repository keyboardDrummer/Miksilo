package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.{KeyGrammar, LanguageGrammars}
import core.deltas.node.Node
import core.deltas.{Contract, DeltaWithGrammar, HasShape}
import core.language.Language
import deltas.bytecode.ByteCodeSkeleton

trait TypeInstance extends DeltaWithGrammar with HasShape with HasType {

  override def inject(language: Language): Unit = {
    TypeSkeleton.getSuperTypesRegistry(language).put(shape, _type => getSuperTypes(_type, language))
    TypeSkeleton.getRegistry(language).instances.put(shape, this)
    super.inject(language)
  }

  def getSuperTypes(_type: Node, state: Language): Seq[Node]

  def getStackType(_type: Node, state: Language): Node = _type

  def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar

  override def dependencies: Set[Contract] = Set(TypeSkeleton, ByteCodeSkeleton)

  def byteCodeGrammarKey = KeyGrammar(shape)
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(shape, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addOption(javaGrammar)

    val byteCodeGrammar = grammars.create(byteCodeGrammarKey, getByteCodeGrammar(grammars))
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(byteCodeGrammar)
  }

  def getJavaGrammar(grammars: LanguageGrammars): BiGrammar
}
