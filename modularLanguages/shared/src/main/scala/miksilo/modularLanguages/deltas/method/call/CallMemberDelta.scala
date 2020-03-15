package miksilo.modularLanguages.deltas.javac.methods.call

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.javac.methods.MemberSelectorDelta

object CallMemberDelta extends DeltaWithGrammar {
  override def dependencies: Set[Contract] = Set(CallDelta, MemberSelectorDelta)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    find(CallDelta.Callee).addAlternative(find(MemberSelectorDelta.Shape))
  }

  override def description = "Enabled calling members using the . operator to select the callee"
}
