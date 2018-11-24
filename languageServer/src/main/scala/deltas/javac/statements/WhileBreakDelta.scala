package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeGrammar, NodeShape}
import core.deltas.path.{NodePath, PathRoot, NodeSequenceElement}
import core.language.{Compilation, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta
import deltas.statement
import deltas.statement.{GotoStatementDelta, StatementDelta, WhileLoopDelta}

import scala.collection.mutable

object WhileBreakDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Adds the break statement to the language"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statementGrammar = language.grammars.find(StatementDelta.Grammar)
    statementGrammar.addAlternative(new NodeGrammar("break" ~ ";", BreakShape))
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    val endLabels = new mutable.HashMap[NodePath, String]()
    PathRoot(program).visitShape(BreakShape, path => transformBreak(path, endLabels, compilation))
  }

  def transformBreak(continuePath: NodePath, endLabels: mutable.Map[NodePath, String], language: Language): Unit = {
    val containingWhile = continuePath.findAncestorShape(WhileLoopDelta.Shape)
    val label = endLabels.getOrElseUpdate(containingWhile, addEndLabel(containingWhile))
    continuePath.replaceData(GotoStatementDelta.neww(label))
  }

  def addEndLabel(whilePath: NodePath): String = {
    val method = whilePath.findAncestorShape(MethodDelta.Shape) //TODO break away from method dependency
    val endLabel = LabelDelta.getUniqueLabel("whileEnd", method)
    whilePath.asInstanceOf[NodeSequenceElement].replaceWith(Seq(whilePath.current, statement.LabelStatementDelta.neww(endLabel)))
    endLabel
  }

  object BreakShape extends NodeShape

  override def dependencies: Set[Contract] = Set(MethodDelta, GotoStatementDelta, WhileLoopDelta)
}
