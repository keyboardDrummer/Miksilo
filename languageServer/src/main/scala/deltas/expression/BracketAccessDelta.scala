package deltas.expression

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Delta, DeltaWithGrammar, Property}
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{ConcreteType, Type, TypeApplication}
import core.smarts.{Constraint, ConstraintBuilder, ConstraintSolver}
import deltas.solidity.MappingTypeDelta

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MappingAccessDelta extends Delta {
  override def description = "Enables accessing mapping members"

  override def inject(language: Language): Unit = {
    BracketAccessDelta.extraConstraints.get(language).insert(0, new HasExtraBracketConstraints {
      override def constraints(compilation: Compilation, builder: ConstraintBuilder,
                               resultType: Type, targetType: Type, indexType: Type, parentScope: Scope): Unit = {
        builder.add(new Constraint() {
          override def apply(solver: ConstraintSolver) = {
            solver.proofs.resolveType(targetType) match {
              case TypeApplication(MappingTypeDelta.mappingTypeConstructor, Seq(keyType, valueType),_) =>
                builder.typesAreEqual(indexType, keyType)
                builder.typesAreEqual(valueType, resultType)
                true
              case _: ConcreteType =>
                true
              case _ => false
            }
          }
        })
      }
    })
    super.inject(language)
  }

  override def dependencies = Set(MappingTypeDelta, BracketAccessDelta)
}



object BracketAccessDelta extends DeltaWithGrammar with ExpressionInstance {

  object Shape extends NodeShape
  object Target extends NodeField
  object Index extends NodeField

  implicit class BracketAccess[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def target: T = node(Target).asInstanceOf[T]
    def index: T = node(Index).asInstanceOf[T]
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val grammar = expression.as(Target) ~ "[" ~ expression.as(Index) ~ "]" asLabelledNode Shape
    expression.addAlternative(grammar)
  }

  override def description = "Enables accessing values from arrays"

  override def dependencies = Set(ExpressionDelta)

  override def shape = Shape

  val extraConstraints = new Property[mutable.ArrayBuffer[HasExtraBracketConstraints]](ArrayBuffer.empty)

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, resultType: Type, parentScope: Scope): Unit = {
    val arrayAccess: BracketAccess[NodePath] = expression
    val targetType = ExpressionDelta.getType(compilation, builder, arrayAccess.target, parentScope)
    val indexType = ExpressionDelta.getType(compilation, builder, arrayAccess.index, parentScope)
    for(extra <- extraConstraints.get(compilation)) {
      extra.constraints(compilation, builder, resultType, targetType, indexType, parentScope)
    }
  }
}

trait HasExtraBracketConstraints {
  def constraints(compilation: Compilation, builder: ConstraintBuilder,
                           resultType: Type,
                           targetType: Type,
                           indexType: Type,
                           parentScope: Scope): Unit

}
