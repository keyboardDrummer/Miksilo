package miksilo.modularLanguages.deltas.statement

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{GrammarKey, Node}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraintsDelta

object StatementDelta extends DeltaWithGrammar {

  override def description: String = "Defines the concept of a statement."

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    grammars.create(Grammar)
  }

  object Grammar extends GrammarKey

  val instances = new ShapeProperty[StatementInstance]

  def getInstance(compilation: Compilation, statement: NodePath): StatementInstance = {
    instances(compilation, statement.shape)
  }

  override def dependencies: Set[Contract] = Set.empty
}

trait StatementInstance extends Delta with HasConstraintsDelta with ControlFlowDelta  { //TODO maybe merge this with ControlFlowDelta

  override def inject(language: Language): Unit = {
    StatementDelta.instances.add(language, this)
    super.inject(language)
  }

  override def dependencies: Set[Contract] = Set(StatementDelta)

  //TODO replace this by using the constraint system.
  def definedVariables(compilation: Compilation, obj: Node): Map[String, Node] = Map.empty
}