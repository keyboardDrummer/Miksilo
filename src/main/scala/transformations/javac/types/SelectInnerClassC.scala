package transformations.javac.types

import core.particles.DeltaWithGrammar
import core.particles.grammars.GrammarCatalogue
import transformations.bytecode.types.ObjectTypeC.ObjectTypeByteCodeGrammarInner

object SelectInnerClassC extends DeltaWithGrammar {

  object SelectInnerClass
  object ParentClass
  object ChildClass
  override def transformGrammars(grammars: GrammarCatalogue): Unit = transformByteCodeGrammars(grammars)

  def transformByteCodeGrammars(grammars: GrammarCatalogue): Unit = {
    val objectInner = grammars.find(ObjectTypeByteCodeGrammarInner)
    val selectInnerGrammar = objectInner <~ "." ~~ objectInner ^^ parseMap(SelectInnerClass, ParentClass, ChildClass)

    objectInner.addOption(selectInnerGrammar)
  }

  override def description: String = "Enables a type referring to an inner class."
}
