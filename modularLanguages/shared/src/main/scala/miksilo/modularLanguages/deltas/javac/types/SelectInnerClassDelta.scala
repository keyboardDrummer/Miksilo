package miksilo.modularLanguages.deltas.javac.types

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{NodeField, NodeShape}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.types.QualifiedObjectTypeDelta
import miksilo.modularLanguages.deltas.bytecode.types.QualifiedObjectTypeDelta.ByteCodeGrammarInner

object SelectInnerClassDelta extends DeltaWithGrammar {

  object SelectInnerClass extends NodeShape
  object ParentClass extends NodeField
  object ChildClass extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = transformByteCodeGrammars(grammars)

  def transformByteCodeGrammars(grammars: LanguageGrammars): Unit = {
    import grammars._
    val objectInner = find(ByteCodeGrammarInner)
    val selectInnerGrammar = (objectInner.as(ParentClass) ~< "." ~~ objectInner.as(ChildClass)).asNode(SelectInnerClass)

    objectInner.addAlternative(selectInnerGrammar)
  }

  override def description: String = "Enables a type referring to an inner class."

  override def dependencies: Set[Contract] = Set(QualifiedObjectTypeDelta)
}
