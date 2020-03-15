package miksilo.modularLanguages.core.deltas.path

import miksilo.editorParser.parsers.core.{EndPointer, StartPointer}
import miksilo.editorParser.parsers.editorParsers.OffsetPointerRange
import miksilo.modularLanguages.core.node.{Key, Node}

case class PathRoot(current: Node) extends NodePath with Key {

  override def parentOption: Option[NodePath] = None

  override def hashCode(): Int = 1 //TODO obj.hashCode

  override def equals(obj: Any): Boolean = obj.isInstanceOf[PathRoot] //TODO && obj.equals..
  override def pathAsString: String = "Root"

  override def rangeOption: Option[OffsetPointerRange] = Some(OffsetPointerRange(StartPointer, EndPointer))
}






