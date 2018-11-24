package core.parsers

import util.cache.Cache

import scala.collection.mutable

class ParseState[Input <: ParseInput](val resultCache: Cache[ParseNode[Input], ParseResult[Input, Any]]) {

  val defaultCache = new DefaultCache()
  val recursionIntermediates = mutable.HashMap[ParseNode[Input], ParseResult[Input, Any]]()
  val callStackSet = mutable.HashSet[ParseNode[Input]]()
  val callStack = mutable.Stack[Parser[Input, _]]()
  var parsersPartOfACycle: Set[Parser[Input, _]] = Set.empty
  val parsersWithBackEdges = mutable.HashSet[Parser[Input, _]]()

  def putIntermediate(key: ParseNode[Input], value: ParseResult[Input, Any]): Unit = {
    recursionIntermediates.put(key, value)
  }

  def removeIntermediate(node: ParseNode[Input]): Unit = {
    recursionIntermediates.remove(node)
  }

  def getPreviousResult[Result](node: ParseNode[Input]): Option[ParseResult[Input, Result]] = {
    if (callStackSet.contains(node)) {
      parsersWithBackEdges.add(node.parser)
      val index = callStack.indexOf(node.parser)
      parsersPartOfACycle ++= callStack.take(index + 1)
      return Some(recursionIntermediates.getOrElse(node,
        ParseFailure[Input, Result](None, node.input, "Traversed back edge without a previous result")).
        asInstanceOf[ParseResult[Input, Result]])
    }
    None
  }

  def withNodeOnStack[T](node: ParseNode[Input], action: () => T): T = {
    callStackSet.add(node)
    callStack.push(node.parser)
    val result = action()
    callStackSet.remove(node)
    callStack.pop()
    result
  }
}

class DefaultCache {
  var values = mutable.Map.empty[Parser[_, _], Option[_]]

  def apply[Result](parser: Parser[_, Result]): Option[Result] = {
    values.get(parser) match {
      case Some(v) =>
        v.asInstanceOf[Option[Result]]
      case None =>
        values.put(parser, None)
        val value = parser.getDefault(this)
        values.put(parser, value)
        value
    }
  }
}
