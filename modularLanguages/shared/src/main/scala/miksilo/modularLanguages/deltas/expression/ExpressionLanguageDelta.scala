package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.modularLanguages.core.SolveConstraintsDelta

//TODO. This delta is WIP and currently just used for testing.
object ExpressionLanguageDelta extends DeltaWithGrammar {
  override def description: String = "Takes an expression as a program and prints the result of that expression"

  override def inject(language: Language): Unit = {
    super.inject(language)
    SolveConstraintsDelta.constraintCollector.add(language, (compilation: Compilation, builder: ConstraintBuilder) => {
      val _type = ExpressionDelta.getType(compilation, builder, compilation.program.asInstanceOf[PathRoot], builder.newScope(debugName = "programScope"))
      //TODO check that the type is printable.
    })
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    find(BodyGrammar).inner = find(ExpressionDelta.FirstPrecedenceGrammar)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)
}
