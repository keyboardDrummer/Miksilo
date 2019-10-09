package deltas.bytecode.types

import core.deltas.grammars.LanguageGrammars
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{Type, TypeFromDeclaration}

object UnqualifiedObjectTypeDelta extends TypeInstance {
  override val shape: Shape.type = Shape

  override def getSuperTypes(_type: Node): Seq[Node] = {
    Seq.empty //TODO extend
  }

  object AnyObjectTypeGrammar extends GrammarKey
  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val result = identifier.as(Name).asLabelledNode(Shape)
    create(AnyObjectTypeGrammar, result | find(QualifiedObjectTypeDelta.Shape))
    result
  }

  def neww(className: String): Node = Shape.create(Name -> className)

  def getName(objectType: NodeLike): String = objectType.getValue(Name).asInstanceOf[String]

  object Name extends NodeField

  object Shape extends NodeShape

  override def description: String = "Defines the object type based on a single identifier."

  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val name = getName(_type)
    TypeFromDeclaration(builder.resolveToType(name, _type.asPath.orNull, parentScope, TypeSkeleton.typeKind))
  }
}
