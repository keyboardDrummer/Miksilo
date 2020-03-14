package core.deltas.path

import core.language.node.{NodeLike, NodeShape}
import languageServer.SourcePath

trait AnyPath extends SourcePath {

  def current: Any
  def uriOption: Option[String]
  def parentOption: Option[NodePath]

  def findAncestorShape(shape: NodeShape): NodePath = ancestors.find(p => p.shape == shape).get
  def ancestors: LazyList[NodePath] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(LazyList.empty)
  def pathAsString: String

  override def toString = s"Path: $pathAsString\nCurrent: $current\nRoot: ${root.current}"
  def root: NodePath = ancestors.last

  override def childElements: Seq[SourcePath] = {
    this match {
      case path: NodePath =>
        path.dataView.values.flatMap((fieldValue: Any) => {
          getSourceElementsFromPath[NodePath](fieldValue)
        }).toSeq
      case _ => Seq.empty
    }
  }

  def getSourceElementsFromPath[Self <: NodeLike](value: Any): Seq[SourcePath] = value match {
    case nodeLike: SourcePath =>
      Seq(nodeLike)
    case sequence: Seq[_] =>
      sequence.collect({ case nodeLikeChild: SourcePath => nodeLikeChild })
    case _ => Seq.empty
  }
}
