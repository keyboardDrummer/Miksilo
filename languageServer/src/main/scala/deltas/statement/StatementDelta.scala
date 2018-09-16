package deltas.statement

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.GrammarKey
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

object StatementDelta extends DeltaWithGrammar {

  override def description: String = "Defines the concept of a statement."

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    grammars.create(Grammar)
  }

  object Grammar extends GrammarKey

  val instances = new ShapeProperty[StatementInstance]

  def getInstance(compilation: Compilation, statement: NodePath): StatementInstance = {
    instances.get(compilation, statement.shape)
  }

  def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {
    getInstance(compilation, statement).constraints(compilation, builder, statement, parentScope)
  }
}

trait StatementInstance extends Delta with HasShape {

  override def inject(language: Language): Unit = {
    StatementDelta.instances.add(language, this)
    super.inject(language)
  }

  override def dependencies: Set[Contract] = Set(StatementDelta)

  def constraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit
}