package core.parsers

import scala.collection.mutable

class ParseState[Input <: ParseInput](val resultCache: Cache[Input]) {

  val defaultCache = new DefaultCache()
  val recursionIntermediates = mutable.HashMap[ParseNode[Input], ParseResult[Input, Any]]()
  val callStackSet = mutable.HashSet[ParseNode[Input]]()
  val callStack = mutable.Stack[Parser[Input, _]]()
  var parsersPartOfACycle: Set[Parser[Input, _]] = Set.empty
  val nodesWithBackEdges = mutable.HashSet[ParseNode[Input]]() //TODO possible this can be only the parsers.

  def putIntermediate(key: ParseNode[Input], value: ParseResult[Input, Any]): Unit = {
    recursionIntermediates.put(key, value)
  }

  def removeIntermediate(node: ParseNode[Input]): Unit = {
    recursionIntermediates.remove(node)
  }

  def getPreviousResult[Result](node: ParseNode[Input]): Option[ParseResult[Input, Result]] = {
    if (callStackSet.contains(node)) {
      nodesWithBackEdges.add(node)
      val index = callStack.indexOf(node.parser)
      parsersPartOfACycle ++= callStack.take(index + 1)
      return Some(recursionIntermediates.getOrElse(node, ParseFailure[Input, Result](None, node.input, "Traversed back edge without a previous result")).
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

trait Cache[Input <: ParseInput] {
  def get(node: ParseNode[Input]): Option[ParseResult[Input, Any]]
  def add(node: ParseNode[Input], value: ParseResult[Input, Any]): Unit
}

class EverythingCache[Input <: ParseInput] extends Cache[Input] {
  val data = mutable.Map[ParseNode[Input], ParseResult[Input, Any]]()

  override def get(node: ParseNode[Input]): Option[ParseResult[Input, Any]] = {
    data.get(node)
  }

  override def add(node: ParseNode[Input], value: ParseResult[Input, Any]): Unit = {
    data.put(node, value)
  }
}

class DefaultCache {
  var values = mutable.Map.empty[Parser[_, _], Option[_]]

  def apply[Result](parser: Parser[_, Result]): Option[Result] = {
    values.getOrElseUpdate(parser, {
      values.put(parser, None)
      parser.getDefault(this)
    }).asInstanceOf[Option[Result]]
  }
}
