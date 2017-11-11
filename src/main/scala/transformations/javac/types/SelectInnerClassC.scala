package transformations.javac.types

import core.particles.grammars.LanguageGrammars
import core.particles.node.{NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import transformations.bytecode.types.ObjectTypeDelta.ObjectTypeByteCodeGrammarInner

object SelectInnerClassC extends DeltaWithGrammar {

  object SelectInnerClass extends NodeClass
  object ParentClass extends NodeField
  object ChildClass extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = transformByteCodeGrammars(grammars)

  def transformByteCodeGrammars(grammars: LanguageGrammars): Unit = {
    import grammars._
    val objectInner = find(ObjectTypeByteCodeGrammarInner)
    val selectInnerGrammar = (objectInner.as(ParentClass) ~< "." ~~ objectInner.as(ChildClass)).asNode(SelectInnerClass)

    objectInner.addOption(selectInnerGrammar)
  }

  override def description: String = "Enables a type referring to an inner class."
}
