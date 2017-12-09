package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}
import core.deltas.path.{Path, PathRoot, SequenceElement}
import deltas.bytecode.simpleBytecode.LabelDelta
import deltas.javac.methods.MethodDelta

import scala.collection.mutable

object WhileBreakDelta extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Adds the break statement to the language"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val statementGrammar = grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.addOption(new NodeGrammar("break;", BreakClazz))
  }

  def transformProgram(program: Node, compilation: Compilation): Unit = {
    val endLabels = new mutable.HashMap[Path, String]()
    PathRoot(program).visitClass(BreakClazz, path => transformBreak(path, endLabels, compilation))
  }

  def transformBreak(continuePath: Path, endLabels: mutable.Map[Path, String], language: Language): Unit = {
    val containingWhile = continuePath.findAncestorClass(WhileLoopDelta.WhileKey)
    val label = endLabels.getOrElseUpdate(containingWhile, addEndLabel(containingWhile))
    continuePath.replaceWith(JustJavaGoto.goto(label))
  }

  def addEndLabel(whilePath: Path): String = {
    val method = whilePath.findAncestorClass(MethodDelta.Clazz)
    val endLabel = LabelDelta.getUniqueLabel("whileEnd", method)
    whilePath.asInstanceOf[SequenceElement].replaceWith(Seq(whilePath.current, JustJavaLabel.label(endLabel)))
    endLabel
  }

  object BreakClazz extends NodeClass
}
