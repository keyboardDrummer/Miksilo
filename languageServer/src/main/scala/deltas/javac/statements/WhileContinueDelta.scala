package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeGrammar, NodeShape}
import core.deltas.path.{NodePath, PathRoot, SequenceElement}
import core.language.{Compilation, Language}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta
import deltas.statement.{StatementDelta, WhileLoopDelta}

import scala.collection.mutable

/*
For each while loop containing a continue, as start label is placed before the while loop, and the continue's are translated to go-to statements that target the start label.
 */
object WhileContinueDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Moves the control flow to the start of the while loop."

  override def dependencies: Set[Contract] = Set(WhileLoopDelta)

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    val startLabels = new mutable.HashMap[NodePath, String]()
    PathRoot(program).visitShape(ContinueKey, path => transformContinue(path, startLabels, compilation))
  }

  def transformContinue(continuePath: NodePath, startLabels: mutable.Map[NodePath, String], language: Language): Unit = {
    val containingWhile = continuePath.findAncestorShape(WhileLoopDelta.Shape)
    val label = startLabels.getOrElseUpdate(containingWhile, addStartLabel(containingWhile))
    continuePath.replaceWith(JustJavaGoto.neww(label))
  }

  def addStartLabel(whilePath: NodePath): String = {
    val method = whilePath.findAncestorShape(MethodDelta.Shape)
    val startLabel = LabelDelta.getUniqueLabel("whileStart", method)
    whilePath.asInstanceOf[SequenceElement].replaceWith(Seq(JustJavaLabel.neww(startLabel), whilePath.current))
    startLabel
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statementGrammar = language.grammars.find(StatementDelta.Grammar)
    statementGrammar.addAlternative(new NodeGrammar("continue" ~ ";", ContinueKey))
  }

  object ContinueKey extends NodeShape
  def continue = new Node(ContinueKey)
}

