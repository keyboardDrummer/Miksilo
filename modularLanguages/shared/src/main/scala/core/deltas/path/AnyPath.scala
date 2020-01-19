package core.deltas.path

import core.language.SourceElement
import core.language.node.{NodeLike, NodeShape}
import core.smarts.ConstraintBuilder
import core.smarts.objects.{DeclarationVariable, NamedDeclaration, Reference}
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

trait AnyPath extends SourceElement {

  def replaceWith(replacement: Any): Unit
  def current: Any
  def uriOption: Option[String]
  def parentOption: Option[NodePath]

  def findAncestorShape(shape: NodeShape): NodePath = ancestors.find(p => p.shape == shape).get
  def ancestors: LazyList[NodePath] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(LazyList.empty)
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
