package core.deltas.path

import core.language.SourceElement
import core.language.node.NodeShape

trait AnyPath extends SourceElement {

  def current: Any
  def uriOption: Option[String]
  def parentOption: Option[NodePath]

  def findAncestorShape(shape: NodeShape): NodePath = ancestors.find(p => p.shape == shape).get
  def ancestors: Stream[NodePath] = parentOption.map(parent => parent #:: parent.ancestors).getOrElse(Stream.empty)
  def pathAsString: String

  override def toString = s"Path: $pathAsString\nCurrent: $current\nRoot: ${root.current}"
  def root: NodePath = ancestors.last
}
