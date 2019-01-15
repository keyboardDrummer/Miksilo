package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.classes.skeleton.HasConstraintsDelta

object UsingForDeclarationDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object LibraryName extends NodeField
  object Type extends NodeField
  object Wildcard

  implicit class UsingFor[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def libraryName: String = node.getValue(LibraryName).asInstanceOf[String]
    def libraryName_=(value: String): Unit = node(LibraryName) = value

    def _type: T = node(Type).asInstanceOf[T]
    def _type_=(value: T): Unit = node(Type) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val grammar = "using" ~~ identifier.as(LibraryName) ~~ "for" ~~
      ("*" ~> value(Wildcard) | typeGrammar).as(Type) ~ ";" asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Add a using-for namespace member"

  override def dependencies = Set(SolidityContractDelta, TypeSkeleton)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val usingFor: UsingFor[NodePath] = path
    val _type = TypeSkeleton.getType(compilation, builder, usingFor._type, parentScope)
    val contractLikeDeclaration = builder.resolve(usingFor.libraryName, usingFor.getSourceElement(LibraryName), parentScope)
    val contractLikeScope = builder.getDeclaredScope(contractLikeDeclaration)

    val typeDeclaration = builder.getDeclarationOfType(_type)
    val typeScope = builder.getDeclaredScope(typeDeclaration)
    builder.importScope(typeScope, contractLikeScope)
  }
}


