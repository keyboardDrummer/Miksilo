package core.language

import core.deltas.node.Node

import scala.collection.mutable
import scala.util.parsing.input.Position

class SourceMap {

//  def findNodeAt(position: Position): Option[Node] = {
//    if (!location.contains(position))
//      return None
//
//    for(child <- children) {
//      val childResult = child.findNodeAt(position)
//      if (child.findNodeAt(position).nonEmpty)
//        return childResult
//    }
//
//    Some(this)
//  }
}

class LanguageServer(language: Language) {

  var program: Option[Node] = None

  def findNodeAt(position: Position): Node = {
    ??? //program.get.findNodeAt(position)
  }

  var state: mutable.Map[Any, Any] = mutable.Map.empty
}
