package deltas.javac.classes.skeleton

import core.deltas.{DeltaWithGrammar, HasShape}
import core.language.node.Node
import core.language.{Compilation, Language}

trait ClassMemberDelta extends DeltaWithGrammar with HasShape {
  def bind(compilation: Compilation, classSignature: ClassSignature, member: Node)
  def compile(compilation: Compilation, member: Node): Unit

  override def inject(language: Language): Unit = {
    JavaClassDelta.members.add(language, this)
    super.inject(language)
  }
}
