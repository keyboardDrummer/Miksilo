package deltas.expression

import core.deltas.Delta
import core.language.{Compilation, Language}
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{ConcreteType, Type, TypeApplication}
import core.smarts.{Constraint, ConstraintBuilder, ConstraintSolver}
import deltas.solidity.MappingTypeDelta

object MappingAccessDelta extends Delta {
  override def description = "Enables accessing mapping members"

  override def inject(language: Language): Unit = {
    BracketAccessDelta.extraConstraints.get(language).insert(0, new HasExtraBracketConstraints {
      override def constraints(compilation: Compilation, builder: ConstraintBuilder,
                               resultType: Type, targetType: Type, indexType: Type, parentScope: Scope): Unit = {
        builder.add(new Constraint() {
          override def apply(solver: ConstraintSolver) = {
            val resolvedType = solver.proofs.resolveType(targetType)
            resolvedType match {
              case TypeApplication(MappingTypeDelta.mappingTypeConstructor, Seq(keyType, valueType),_) =>
                builder.typesAreEqual(indexType, keyType)
                builder.typesAreEqual(valueType, resultType)
                true
              case _: ConcreteType =>
                true
              case _ =>
                false
            }
          }
        })
      }
    })
    super.inject(language)
  }

  override def dependencies = Set(MappingTypeDelta, BracketAccessDelta)
}
