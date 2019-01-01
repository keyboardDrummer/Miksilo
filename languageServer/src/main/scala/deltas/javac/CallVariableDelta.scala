package deltas.javac

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.expressions.VariableDelta
import deltas.javac.methods.call.CallDelta

object CallVariableDelta extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val callee = grammars.find(CallDelta.Callee)
    callee.addAlternative(grammars.find(VariableDelta.Shape))
  }

  override def description = "Allow calling a variable as a method."

  override def dependencies = Set(CallDelta, VariableDelta)
}
