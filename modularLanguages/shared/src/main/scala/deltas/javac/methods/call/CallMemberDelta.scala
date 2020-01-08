package deltas.javac.methods.call

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.javac.methods.MemberSelectorDelta

object CallMemberDelta extends DeltaWithGrammar {
  override def dependencies: Set[Contract] = Set(CallDelta, MemberSelectorDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    find(CallDelta.Callee).addAlternative(find(MemberSelectorDelta.Shape))
  }

  override def description = "Enabled calling members using the . operator to select the callee"
}
