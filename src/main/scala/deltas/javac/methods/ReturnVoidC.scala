package deltas.javac.methods

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}
import core.deltas.path.Path
import deltas.bytecode.coreInstructions.VoidReturnInstructionDelta
import deltas.javac.statements.{StatementInstance, StatementSkeleton}

object ReturnVoidC extends StatementInstance {

  override def dependencies: Set[Contract] = Set(MethodDelta, VoidReturnInstructionDelta)

  override def getNextStatements(obj: Path, labels: Map[Any, Path]): Set[Path] = Set.empty

  def returnToLines(_return: Node, compiler: MethodCompiler): Seq[Node] = {
    Seq(VoidReturnInstructionDelta.voidReturn)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statement = find(StatementSkeleton.StatementGrammar)

    val returnExpression = ("return" ~ ";") ~> value(_return)
    statement.inner = statement.inner | returnExpression
  }

  def _return: Node = new Node(ReturnVoidKey)

  object ReturnVoidKey extends NodeClass

  override val key = ReturnVoidKey

  override def toByteCode(_return: Path, compilation: Compilation): Seq[Node] = {
    Seq(VoidReturnInstructionDelta.voidReturn)
  }

  override def description: String = "Allows returning void."
}
