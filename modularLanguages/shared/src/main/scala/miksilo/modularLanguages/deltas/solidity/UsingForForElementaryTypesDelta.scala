package miksilo.modularLanguages.deltas.solidity

import miksilo.modularLanguages.core.deltas.Delta
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.NodeLike
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{Type, TypeFromDeclaration}
import miksilo.modularLanguages.deltas.bytecode.types.{HasType, TypeSkeleton}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasConstraints
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, HasNameDelta}

object UsingForForElementaryTypesDelta extends Delta {
  override def description = "Allow the for-using contruct to be used with elementary types"

  override def dependencies = Set(
    ElementaryTypeDelta,
    MultiFileDelta // TODO override the root hasConstraints instead of hardcoding MultiFile
    )

  override def inject(language: Language): Unit = {
    TypeSkeleton.hasTypes.add(language, ElementaryTypeDelta.Shape, new HasType {
      override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope): Type = {
        val name = path.getValue(HasNameDelta.Name).asInstanceOf[String]
        val typeDeclaration = builder.resolveToType(name, null, parentScope, TypeSkeleton.typeKind)
        TypeFromDeclaration(typeDeclaration)
      }
    })

    val original = ConstraintSkeleton.hasConstraints.get(language, MultiFileDelta.Shape).get
    ConstraintSkeleton.hasConstraints.add(language, MultiFileDelta.Shape, new HasConstraints {
      override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
        for(name <- ElementaryTypeDelta.elementaryTypeNames) {
          if (name != "address") {
            val declaration = builder.declare(name, parentScope, null, Some(TypeSkeleton.typeKind))
            builder.declareScope(declaration)
          }
        }
        original.collectConstraints(compilation, builder, path, parentScope)
      }
    })
  }
}
