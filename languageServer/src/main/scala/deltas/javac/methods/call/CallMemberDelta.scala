package deltas.javac.methods.call

import core.deltas.path.NodePath
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Reference
import core.smarts.scopes.objects.Scope
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.call.CallDelta.Call

object CallMemberDelta extends DeltaWithGrammar {
  override def dependencies: Set[Contract] = Set(CallDelta, MemberSelectorDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    find(CallDelta.Callee).addAlternative(find(MemberSelectorDelta.Shape))
  }

  override def description = "Enabled calling members using the . operator to select the callee"
}
