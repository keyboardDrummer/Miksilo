package deltas.statement

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.GrammarKey
import core.language.{Compilation, Language}
import deltas.javac.classes.skeleton.HasConstraintsDelta

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
}

trait StatementInstance extends Delta with HasConstraintsDelta {

  override def inject(language: Language): Unit = {
    StatementDelta.instances.add(language, this)
    super.inject(language)
  }

  override def dependencies: Set[Contract] = Set(StatementDelta)
}