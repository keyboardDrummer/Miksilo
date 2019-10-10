package core.deltas.path

import core.language.SourceElement
import core.language.node.{NodeLike, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.objects.{DeclarationVariable, NamedDeclaration, Reference}
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

object ConstraintBuilderExtension {
  implicit class ExtendedConstraintBuilder(builder: ConstraintBuilder) {

    def resolveToType(origin: AnyPath, scope: Scope, _type: Type) : DeclarationVariable = {
      builder.resolveToType(origin.current.asInstanceOf[String], origin, scope, _type)
    }

    def referSourceElement(hasName: AnyPath, scope: Scope): Reference = {
      builder.refer(hasName.current.asInstanceOf[String], scope, Some(hasName))
    }

    def declare(hasName: AnyPath, container: Scope, _type: Type): NamedDeclaration = {
      builder.declare(hasName.current.asInstanceOf[String], container, hasName, Some(_type))
    }

    def declare(name: AnyPath, container: Scope): NamedDeclaration = {
      builder.declare(name.current.asInstanceOf[String], container, name, None)
    }
  }
}
trait AnyPath extends SourceElement {

  def current: Any
  def uriOption: Option[String]
  def parentOption: Option[NodePath]

  def findAncestorShape(shape: NodeShape): NodePath = ancestors.find(p => p.shape == shape).get
  def ancestors: Stream[NodePath] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def pathAsString: String

  override def toString = s"Path: $pathAsString\nCurrent: $current\nRoot: ${root.current}"
  def root: NodePath = ancestors.last

  override def childElements: Seq[SourceElement] = {
    this match {
      case path: NodePath =>
        path.dataView.values.flatMap((fieldValue: Any) => {
          getSourceElementsFromPath[NodePath](fieldValue)
        }).toSeq
      case _ => Seq.empty
    }
  }

  def getSourceElementsFromPath[Self <: NodeLike](value: Any): Seq[SourceElement] = value match {
    case nodeLike: SourceElement =>
      Seq(nodeLike)
    case sequence: Seq[_] =>
      sequence.collect({ case nodeLikeChild: SourceElement => nodeLikeChild })
    case _ => Seq.empty
  }
}
