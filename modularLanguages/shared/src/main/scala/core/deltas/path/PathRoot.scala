package core.deltas.path

import core.language.{Compilation, SourceElement}
import core.language.node.{Key, Node}
import core.parsers.editorParsers.{Position, SourceRange}

object PathRoot {
  implicit def fromCompilation(compilation: Compilation): PathRoot = compilation.program.asInstanceOf[PathRoot]
}

case class PathRoot(compilation: Compilation, current: Node) extends NodePath with Key {

  override def parentOption: Option[NodePath] = None

  override def hashCode(): Int = 1 //TODO obj.hashCode

  override def equals(obj: Any): Boolean = obj.isInstanceOf[PathRoot] //TODO && obj.equals..
  override def pathAsString: String = "Root"

  override def range: Option[SourceRange] = Some(SourceRange(new Position(0, 0), new Position(Int.MaxValue, Int.MaxValue)))

  override def replaceWith(replacement: Any): Unit = compilation.program = PathRoot(compilation, replacement.asInstanceOf[Node])
}






