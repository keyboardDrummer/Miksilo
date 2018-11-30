package core.parsers.editorParsers

import scala.collection.mutable

class DefaultCache {
  var values = mutable.Map.empty[HasGetDefault[Any], Option[_]]

  def apply[Result](parser: HasGetDefault[Result]): Option[Result] = {
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

trait HasGetDefault[+Result] {
  def getDefault(cache: DefaultCache): Option[Result]

}