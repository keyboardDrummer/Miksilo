package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.node.Node
import core.deltas.{Contract, DeltaWithGrammar, HasShape}
import core.language.Language
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.types.TypeSkeleton.HasSuperTypes

trait TypeInstance extends DeltaWithGrammar with HasShape with HasType with HasSuperTypes {

  override def inject(language: Language): Unit = {
    TypeSkeleton.hasSuperTypes.add(language, this)
    TypeSkeleton.typeInstances.add(language, this)
    super.inject(language)
  }

  def getSuperTypes(_type: Node): Seq[Node]

  override def dependencies: Set[Contract] = Set(TypeSkeleton, ByteCodeSkeleton)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(shape, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addAlternative(javaGrammar)
  }

  def getJavaGrammar(grammars: LanguageGrammars): BiGrammar
}
