package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.deltas.Delta
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{ConcreteType, Type, TypeApplication}
import miksilo.languageServer.core.smarts.{Constraint, ConstraintBuilder, ConstraintSolver}
import miksilo.modularLanguages.deltas.solidity.MappingTypeDelta

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
