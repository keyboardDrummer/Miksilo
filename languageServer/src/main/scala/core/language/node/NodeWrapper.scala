package core.language.node

trait NodeWrapper[T <: NodeLike] {
  def node: T

  def get(key: NodeField): Option[Any] = node.get(key)
  def apply(key: NodeField): Any = node.apply(key)
  def update(key: NodeField, value: Any): Unit = node.update(key, value)
  def shape: NodeShape = node.shape
  def shape_=(value: NodeShape): Unit = node.shape = value
  def dataView: Map[NodeField, Any] = node.dataView
}

object NodeWrapper
{
  implicit def wrapList[TNodeWrapper, T <: NodeLike](list: Seq[T])(implicit wrap: T => TNodeWrapper): Seq[TNodeWrapper] =
    list.map(n => wrap(n))
  implicit def unwrapList[T <: NodeLike](list: Seq[NodeWrapper[T]]): Seq[T] = list.map(n => n.node)
  implicit def unwrap[T <: NodeLike] (wrapper: NodeWrapper[T]): T = wrapper.node
}