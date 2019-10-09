package deltas.verilog

import core.deltas.NodeGrammarWriter
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import deltas.verilog.AlwaysDelta.SensitivityVariables

object SensitivityVariableDelta {
  object Shape extends NodeShape
  object Edge extends NodeField
  object Name extends NodeField

  implicit class SensitivityVariable[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def name: String = node.getValue(Name).asInstanceOf[String]
    def edge: String = node.getValue(Edge).asInstanceOf[String]
  }

  def neww(edge: String, name: String): SensitivityVariable[Node] = Shape.create(Edge -> edge, Name -> name)

  def getListGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._

    val edge: BiGrammar = stringToGrammar("posedge") | "negedge" | ValueGrammar("")
    val element: BiGrammar = edge.as(Edge) ~~ identifier.as(Name) asNode Shape
    val sensitivityList: BiGrammar = "@" ~ element.manySeparated("," | "or").as(SensitivityVariables).inParenthesis
    sensitivityList
  }
}
