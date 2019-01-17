package deltas.solidity

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton
import deltas.javac.classes.skeleton.HasConstraintsDelta
import deltas.solidity.HasNameDelta.HasName
import deltas.statement.LocalDeclarationDelta

object HasNameDelta extends DeltaWithGrammar {
  object Name extends NodeField

  class HasName[T <: NodeLike](val node: T) extends NodeWrapper[T] {

    def name: String = node.getValue(Name).asInstanceOf[String]
    def name_=(value: String): Unit = node(Name) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    grammars.create(Name, identifier.as(Name))
  }

  override def description = "Introduces the concept of a name"

  override def dependencies = Set.empty
}

object StructDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object Members extends NodeField

  implicit class Struct[T <: NodeLike](node: T) extends HasName[T](node) {
    def members = node(Members).asInstanceOf[Seq[T]]
    def members_=(value: Seq[T]): Unit = node(Members) = value
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val declaration = find(LocalDeclarationDelta.Shape)
    val grammar = "struct" ~~ find(HasNameDelta.Name) ~ "{" % declaration.manyVertical.as(Members) % "}" asNode Shape
    find(SolidityContractDelta.Members).addAlternative(grammar)
  }

  override def description = "Adds solidity structs"

  override def dependencies = Set(SolidityContractDelta)

  override def shape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val struct: Struct[NodePath] = path
    val declaration = builder.declare2(path.getSourceElement(HasNameDelta.Name), parentScope)
    val structScope = builder.declareScope(declaration, Some(parentScope), s"struct '${struct.name}'")
    for(member <- struct.members) {
      ConstraintSkeleton.constraints(compilation, builder, member, structScope)
    }
  }
}
