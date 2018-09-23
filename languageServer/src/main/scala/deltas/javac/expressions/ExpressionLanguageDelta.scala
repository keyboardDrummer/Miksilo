package deltas.javac.expressions

import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.path.PathRoot
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import deltas.expressions.ExpressionDelta

//TODO. This delta is WIP and currently just used for testing.
object ExpressionLanguageDelta extends DeltaWithGrammar {
  override def description: String = "Takes an expression as a program and prints the result of that expression"

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.collectConstraints = (compilation: Compilation, builder: ConstraintBuilder) => {
      val _type = ExpressionDelta.getType(compilation, builder, PathRoot(compilation.program), builder.newScope(debugName = "programScope"))
      //TODO check that the type is printable.
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    find(BodyGrammar).inner = find(ExpressionDelta.FirstPrecedenceGrammar)
  }

  override def dependencies: Set[Contract] = Set(ExpressionDelta)
}
