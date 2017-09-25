package transformations.javac.types

import core.particles.{Language, DeltaWithGrammar}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Key
import transformations.bytecode.types.ObjectTypeC.ObjectTypeByteCodeGrammarInner

object SelectInnerClassC extends DeltaWithGrammar {

  object SelectInnerClass extends Key
  object ParentClass extends Key
  object ChildClass extends Key
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = transformByteCodeGrammars(grammars)

  def transformByteCodeGrammars(grammars: GrammarCatalogue): Unit = {
    val objectInner = grammars.find(ObjectTypeByteCodeGrammarInner)
    val selectInnerGrammar = (objectInner <~ "." ~~ objectInner).asNode(SelectInnerClass, ParentClass, ChildClass)

    objectInner.addOption(selectInnerGrammar)
  }

  override def description: String = "Enables a type referring to an inner class."
}
