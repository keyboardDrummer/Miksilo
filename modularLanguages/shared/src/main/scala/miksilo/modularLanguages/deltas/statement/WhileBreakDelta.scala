package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.{NodePath, NodeSequenceElement, PathRoot}
import miksilo.modularLanguages.core.node.{Node, NodeGrammar, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.statement.{GotoStatementDelta, LabelStatementDelta, StatementDelta, WhileLoopDelta}

import scala.collection.mutable

object WhileBreakDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Adds the break statement to the language"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statementGrammar = find(StatementDelta.Grammar)
    statementGrammar.addAlternative(new NodeGrammar("break" ~ ";", BreakShape))
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    val endLabels = new mutable.HashMap[NodePath, String]()
    PathRoot(program).visitShape(BreakShape, path => transformBreak(compilation, path, endLabels))
  }

  def transformBreak(compilation: Compilation, breakPath: NodePath, endLabels: mutable.Map[NodePath, String]): Unit = {
    val containingWhile = breakPath.findAncestorShape(WhileLoopDelta.Shape)
    val label = endLabels.getOrElseUpdate(containingWhile, addEndLabel(compilation, containingWhile))
    breakPath.replaceData(GotoStatementDelta.neww(label))
  }

  def addEndLabel(compilation: Compilation, whilePath: NodePath): String = {
    val endLabel = LabelStatementDelta.getUniqueLabel(compilation, "whileEnd", whilePath)
    whilePath.asInstanceOf[NodeSequenceElement].replaceWith(Seq(whilePath.current, LabelStatementDelta.neww(endLabel)))
    endLabel
  }

  object BreakShape extends NodeShape

  override def dependencies: Set[Contract] = Set(GotoStatementDelta, WhileLoopDelta)
}
