package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.Node
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

  override def dependencies: Set[Contract] = Set(TypeSkeleton, ByteCodeSkeleton)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(shape, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addOption(javaGrammar)
  }

  def getJavaGrammar(grammars: LanguageGrammars): BiGrammar
}
