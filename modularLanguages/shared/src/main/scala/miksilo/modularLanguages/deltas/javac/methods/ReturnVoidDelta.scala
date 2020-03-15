package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.VoidReturnInstructionDelta
import miksilo.modularLanguages.deltas.statement.{StatementDelta, StatementInstance}

object ReturnVoidDelta extends StatementInstance with DeltaWithGrammar  {

  override def dependencies: Set[Contract] = Set(MethodDelta)

  def returnToLines(_return: Node, compiler: MethodCompiler): Seq[Node] = {
    Seq(VoidReturnInstructionDelta.voidReturn)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val statement = find(StatementDelta.Grammar)

    val returnExpression = ("return" ~ ";") ~> value(_return)
    statement.inner = statement.inner | returnExpression
  }

  def _return: Node = new Node(Shape)

  object Shape extends NodeShape

  override val shape = Shape

  override def description: String = "Allows returning void."

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, statement: NodePath, parentScope: Scope): Unit = {}
}
