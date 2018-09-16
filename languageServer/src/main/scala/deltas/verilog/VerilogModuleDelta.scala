package deltas.verilog

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.language.Language
import core.language.node.{Node, NodeField, NodeShape}

object VerilogModuleDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField
  object Parameters extends NodeField
  object Body extends NodeField

  object MemberShape extends NodeShape

  def neww(name: String, parameters: Seq[String], body: Seq[Node]): Node = Shape.create(
    Name -> name,
    Parameters -> parameters,
    Body -> body)

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val member = create(MemberShape)
    val parameterList: BiGrammar = identifier.manySeparatedVertical(",").as(Parameters).inParenthesis
    val body: BiGrammar = member.manyVertical.as(Body)
    val moduleGrammar: BiGrammar = "module" ~~ identifier.as(Name) ~~ parameterList ~ ";" % body.indent() % "endmodule" asNode Shape
    find(BodyGrammar).inner = moduleGrammar
  }

  override def description: String = "Adds the Verilog module"
}



