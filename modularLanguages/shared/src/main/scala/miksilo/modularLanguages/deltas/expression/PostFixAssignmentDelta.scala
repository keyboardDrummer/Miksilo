package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta
import miksilo.modularLanguages.deltas.statement.assignment.SimpleAssignmentDelta

trait PostFixAssignmentDelta extends DeltaWithGrammar with ExpressionInstance {

  def keyword: String

  override val shape = Shape

  override def dependencies: Set[Contract] = Set(SimpleAssignmentDelta, ExpressionDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val target = find(SimpleAssignmentDelta.Target)
    val postFixIncrement = target ~< keyword asNode Shape
    find(ExpressionDelta.LastPrecedenceGrammar).addAlternative(postFixIncrement)
  }

  object Shape extends NodeShape

  override def description: String = s"Adds the postfix $keyword operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, plusPlus: NodePath, _type: Type, parentScope: Scope): Unit = {
    builder.typesAreEqual(IntTypeDelta.constraintType, _type)
    val target = plusPlus(SimpleAssignmentDelta.Target).asInstanceOf[NodePath]
    ExpressionDelta.addConstraints(compilation, builder, target, _type, parentScope)
  }
}


