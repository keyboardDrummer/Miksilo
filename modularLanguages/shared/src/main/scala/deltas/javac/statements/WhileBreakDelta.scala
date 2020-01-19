package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, NodeSequenceElement, PathRoot}
import core.language.node.{Node, NodeGrammar, NodeShape}
import core.language.{Compilation, Language}
import deltas.statement.{GotoStatementDelta, LabelStatementDelta, StatementDelta, WhileLoopDelta}

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
    PathRoot.fromCompilation(compilation).visitShape(BreakShape, path => transformBreak(compilation, path, endLabels))
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
