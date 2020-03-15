package miksilo.modularLanguages.deltas.verilog

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.grammars.ValueGrammar
import miksilo.modularLanguages.core.deltas.NodeGrammarWriter
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.verilog.AlwaysDelta.SensitivityVariables

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
