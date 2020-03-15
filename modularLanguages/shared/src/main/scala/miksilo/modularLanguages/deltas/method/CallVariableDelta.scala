package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.expression.VariableDelta
import miksilo.modularLanguages.deltas.javac.methods.call.CallDelta

object CallVariableDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val callee = grammars.find(CallDelta.Callee)
    callee.addAlternative(grammars.find(VariableDelta.Shape))
  }

  override def description = "Allow calling a variable as a method."

  override def dependencies = Set(CallDelta, VariableDelta)
}
