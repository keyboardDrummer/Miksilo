package deltas.expression

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.IntTypeDelta
import deltas.statement.assignment.SimpleAssignmentDelta

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
    ExpressionDelta.getConstraints(compilation, builder, target, _type, parentScope)
  }
}


