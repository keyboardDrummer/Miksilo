package deltas.verilog

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object VerilogModuleDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField
  object Parameters extends NodeField
  object Body extends NodeField

  object MemberShape extends NodeShape
  object StatementShape extends NodeShape

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val statement = create(StatementShape)
    val member = create(MemberShape)
    member.addAlternative(statement)
    val parameterList: BiGrammar = identifier.manySeparatedVertical(",").as(Parameters).inParenthesis
    val body: BiGrammar = member.manyVertical.as(Body)
    val moduleGrammar: BiGrammar = "module" ~~ identifier.as(Name) ~~ parameterList ~ ";" % body.indent() % "endmodule"
    find(BodyGrammar).inner = moduleGrammar
  }

  override def description: String = "Adds the Verilog module"
}

object VariableTypeSpecifier extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Variables extends NodeField
  object Type extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val member = find(VerilogModuleDelta.MemberShape)

    val _type: BiGrammar = "input" | "output" | "reg"
    val input: BiGrammar = _type.as(Type) ~~ identifier.manySeparated(",").as(Variables) asNode Shape
    member.addAlternative(input)
  }

  override def description: String = "Adds the input/output/reg members"
}

object SensitivityVariable {
  object Shape extends NodeShape
  object Edge extends NodeField
  object Name extends NodeField
}

object AlwaysDelta extends DeltaWithGrammar {
  object Shape extends NodeShape
  object SensitivityVariables extends NodeField
  object Body extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val statement = find(VerilogModuleDelta.StatementShape)
    val edge: BiGrammar = "posedge" | "negedge" | ""
    val variable: BiGrammar = edge.as(SensitivityVariable.Edge) ~~ identifier.as(SensitivityVariable.Name) asNode SensitivityVariable.Shape
    val sensitivityList: BiGrammar = "@" ~ variable.manySeparated(",").as(SensitivityVariables)
    val always: BiGrammar = "always" ~~ sensitivityList % statement.as(Body) asNode Shape
    statement.addAlternative(always)
  }

  override def description: String = "Adds the always statement"
}

object BeginEnd extends DeltaWithGrammar {
  object Shape extends NodeShape
  object Statements extends NodeField
  object Label extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val statement = find(VerilogModuleDelta.StatementShape)
    val label = (":" ~~< identifier).option.as(Label)
    val beginEnd = "begin" ~ label ~ (statement ~< ";").manyVertical.as(Statements) ~ "end"
    statement.addAlternative(beginEnd)
  }

  override def description: String = "Adds the begin end block"
}