package deltas.yaml

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.{BiSequence, RegexGrammar, SequenceBijective}
import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import deltas.expression.ExpressionDelta
import deltas.json.StringLiteralDelta

object PlainScalarDelta extends DeltaWithGrammar {
  def flowIndicatorChars = """,\[\]{}"""

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    val _grammars = grammars
    import grammars._

    val nonBreakChars = """\n"""
    val plainSafeOutChars = s"""$nonBreakChars#'"""
    val plainSafeInChars = s"""$plainSafeOutChars$flowIndicatorChars"""
    val doubleColonPlainSafeIn =  RegexGrammar(s"""([^$plainSafeInChars:]|:[^$plainSafeInChars ])+""".r, "plain scalar")
    val doubleColonPlainSafeOut =  RegexGrammar(s"""([^$plainSafeOutChars:]|:[^$plainSafeOutChars ])+""".r, "plain scalar")

    val nsPlainSafe: BiGrammar = new IfContext(Map(
      FlowIn -> doubleColonPlainSafeIn,
      FlowOut -> doubleColonPlainSafeOut,
      BlockKey -> doubleColonPlainSafeOut,
      FlowKey -> doubleColonPlainSafeIn), doubleColonPlainSafeOut)

    val plainStyleSingleLineString: BiGrammar = nsPlainSafe
    val plainStyleMultiLineString: BiGrammar = {
      val firstLine = new BiSequence(nsPlainSafe, _grammars.trivia, BiSequence.ignoreRight, false)
      new BiSequence(firstLine,
        CheckIndentationGrammar.greaterThan(new WithIndentationGrammar(CheckIndentationGrammar.equal(nsPlainSafe).someSeparated("\n"))),
        SequenceBijective((firstLine: Any, rest: Any) => {
          firstLine.asInstanceOf[String] + rest.asInstanceOf[List[String]].fold("")((a, b) => a + " " + b)
        }, (value: Any) => Some(value, List.empty)), false)
    }

    val plainScalar: BiGrammar = new WithContext({
      case FlowIn => FlowIn
      case BlockKey => BlockKey
      case FlowKey => FlowKey
      case _ => FlowOut
    }, plainStyleMultiLineString | plainStyleSingleLineString).
      as(StringLiteralDelta.Value).asLabelledNode(StringLiteralDelta.Shape)

    find(ExpressionDelta.FirstPrecedenceGrammar).addAlternative(plainScalar)

  }

  override def description = "Adds the YAML plain scalar"

  override def dependencies = Set.empty
}
