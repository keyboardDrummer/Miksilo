package deltas.solidity

import core.deltas.Delta
import core.deltas.path.NodePath
import core.language.node.NodeLike
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{Type, TypeFromDeclaration}
import deltas.bytecode.types.{HasType, TypeSkeleton}
import deltas.javac.classes.skeleton.HasConstraints
import deltas.{ConstraintSkeleton, HasNameDelta}

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
        val typeDeclaration = builder.resolveToType(name, null, parentScope, ElementaryTypeDelta.elementaryTypeKind)
        TypeFromDeclaration(typeDeclaration)
      }
    })

    val original = ConstraintSkeleton.hasConstraints.get(language, MultiFileDelta.Shape).get
    ConstraintSkeleton.hasConstraints.add(language, MultiFileDelta.Shape, new HasConstraints {
      override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
        for(name <- ElementaryTypeDelta.elementaryTypeNames) {
          val declaration = builder.declare(name, parentScope, null, Some(ElementaryTypeDelta.elementaryTypeKind))
          builder.declareScope(declaration)
        }
        original.collectConstraints(compilation, builder, path, parentScope)
      }
    })
  }
}
