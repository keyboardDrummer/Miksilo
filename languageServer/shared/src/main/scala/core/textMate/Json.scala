package core.textMate

trait JsExpression
case class JsArray(elements: Seq[JsExpression]) extends JsExpression
case class JsObject(fields: Map[String, JsExpression]) extends JsExpression
case class JsLiteral(value: String) extends JsExpression

object Json {

  def printJson(root: JsExpression): String = {
    val builder = new StringBuilder
    var indentation = 0

    def addMember(name: String, value: JsExpression): Unit = {
      builder.append("\"" + name + "\"").append(": ")
      add(value)
    }

    def add(node: JsExpression): Unit = {
      def addNewLine() = builder.append("\n" + " " * indentation)
      def indent(): Unit = indentation += 2
      def dedent(): Unit = indentation -= 2

      node match {
        case array: JsArray =>
          builder.append("[")
          if (array.elements.nonEmpty) {
            indent()
            addNewLine()
            for(element <- array.elements.dropRight(1)) {
              add(element)
              builder.append(",")
              addNewLine()
            }
            add(array.elements.last)
            dedent()
          }
          addNewLine()
          builder.append("]")

        case obj: JsObject =>
          builder.append("{")

          if (obj.fields.nonEmpty) {
            indent()
            addNewLine()
            for(member <- obj.fields.dropRight(1)) {
              addMember(member._1, member._2)
              builder.append(",")
              addNewLine()
            }
            addMember(obj.fields.last._1, obj.fields.last._2)
            dedent()
            addNewLine()
          }
          builder.append("}")

        case literal: JsLiteral =>
          builder.append(stringToStringLiteral(literal.value))
      }
    }
    add(root)
    builder.toString()
  }

  private def stringToStringLiteral(value: String) = {
    "\"" + value.
      replace("\\", "\\\\").
      replace("\"", "\\\"") + "\""
  }
}
