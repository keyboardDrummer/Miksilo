package miksilo.modularLanguages.deltas.javac.statements

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.deltas.javac.expressions.ConvertsToByteCodeDelta
import miksilo.modularLanguages.deltas.statement.LocalDeclarationDelta

object LocalDeclarationDeltaToByteCode extends ConvertsToByteCodeDelta {

  //TODO instead of compiling to BC, add a phase that removes this statment.
  // Problem is our (too) big class/method transformation phase does both the locals analysis and the compilation to bytecode, so we can't get in between there,
  override def toByteCode(declaration: NodePath, compilation: Compilation): Seq[Node] = {
    Seq.empty[Node]
  }

  override def description: String = "Compiles local declarations to bytecode"

  override def shape: NodeShape = LocalDeclarationDelta.Shape

  override def dependencies: Set[Contract] = Set(LocalDeclarationDelta)
}
