package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.GrammarKey
import core.deltas.path.NodePath
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

object BlockDelta extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(StatementSkeleton)

  val indentAmount = 4
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementSkeleton.StatementGrammar)
    val blockGrammar = create(Grammar, "{" %> statementGrammar.manyVertical.indent(indentAmount) %< "}")
    val statementAsBlockGrammar = create(StatementAsBlockGrammar, statementGrammar.map[Any, Seq[Any]](statement => Seq(statement), x => x.head))
    create(BlockOrStatementGrammar, blockGrammar | statementAsBlockGrammar)
  }

  def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statements: Seq[NodePath], parentScope: Scope): Unit = {
    for(statement <- statements) {
      StatementSkeleton.constraints(compilation, builder, statement, parentScope)
    }
  }

  object BlockOrStatementGrammar extends GrammarKey
  object StatementAsBlockGrammar extends GrammarKey
  object Grammar extends GrammarKey

  override def description: String = "Defines a grammar for blocks."
}
