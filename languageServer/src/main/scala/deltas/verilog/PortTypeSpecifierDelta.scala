package deltas.verilog

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object PortTypeSpecifierDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Variables extends NodeField
  object Type extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val member = find(VerilogModuleDelta.MemberShape)

    val _type: BiGrammar = "input" | "output" | "reg"
    val input: BiGrammar = _type.as(Type) ~~ identifier.manySeparated(",").as(Variables) ~ ";" asNode Shape
    member.addAlternative(input)
  }

  override def description: String = "Adds the input/output/reg members"
}
