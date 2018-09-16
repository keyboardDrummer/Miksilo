package deltas.verilog

import core.bigrammar.BiGrammar
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.language.Language
import core.language.node.{NodeField, NodeShape}
import deltas.expressions.ExpressionDelta
import deltas.statement.StatementDelta

object VerilogModuleDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Name extends NodeField
  object Parameters extends NodeField
  object Body extends NodeField

  object MemberShape extends NodeShape

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

object NonBlockingAssignmentDelta extends DeltaWithGrammar { //TODO merge with AssignmentSkeleton
  object Shape extends NodeShape
  object Target extends NodeField
  object Value extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val statement = find(StatementDelta.Grammar)
    val assignment: BiGrammar = identifier.as(Target) ~~ "<=" ~~ expression.as(Value) asNode Shape
    statement.addAlternative(assignment)
  }

  override def description: String = "Adds the non-blocking assignment operator <="
}

