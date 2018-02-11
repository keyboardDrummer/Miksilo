package deltas.javac.classes.skeleton

import core.deltas.DeltaWithGrammar
import core.language.node.Node
import core.language.{Compilation, Language}

trait ClassMemberDelta extends DeltaWithGrammar {
  def bind(compilation: Compilation, classSignature: ClassSignature, member: Node)
  def compile(compilation: Compilation, member: Node): Unit

  override def inject(language: Language): Unit = {
    JavaClassSkeleton.getRegistry(language).members ::= this
    super.inject(language)
  }
}
