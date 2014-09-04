package core.grammarDocument

import java.util.Objects

import core.document.Empty
import core.grammar.~
import core.responsiveDocument.{ResponsiveDocument, WrappedSizedDocument}

object PrintValueUsingGrammarDocument {

  def deSequence(value: Any) = value match {
    case ~(_, _) => Some(value)
    case MissingValue => Some(core.grammar.~(MissingValue, MissingValue))
    case _ => None
  }

  def toDocument(value: Any, grammar: GrammarDocument): ResponsiveDocument = {
    var labelledValues: Map[Labelled, Any] = Map.empty

    def helper(value: Any, grammar: GrammarDocument): Option[ResponsiveDocument] = {
      grammar.simplify match {
        case Sequence(first, second) =>
          for {
            ~(firstValue, secondValue) <- deSequence(value)
            firstDoc <- helper(firstValue, first)
            secondDoc <- helper(secondValue, second)
          } yield firstDoc ~ secondDoc
        case Choice(first, second) => helper(value, first).orElse(helper(value, second))
        case Consume(consume) => Some(value.toString)
        case Keyword(keyword) => Some(keyword)
        case Delimiter(keyword) => Some(keyword)
        case labelled: Labelled =>
          if (labelledValues.get(labelled).exists(v => v.equals(value)))
            return None
          labelledValues += labelled -> value
          helper(value, labelled.inner)
        case Many(inner) =>
          val documentOptions: Seq[Option[ResponsiveDocument]] = value.asInstanceOf[Seq[Any]].map(element => helper(element, inner))
          if (documentOptions.exists(_ isEmpty)) None
          else Some(documentOptions.map(_.get).fold[ResponsiveDocument](Empty)((a, b) => a ~ b))
        case MapGrammar(inner, _, deconstruct) => deconstruct(value).flatMap(value => helper(value, inner))
        case TopBottom(top, bottom) =>
          for {
            ~(firstValue, secondValue) <- deSequence(value)
            firstDoc <- helper(firstValue, top)
            secondDoc <- helper(secondValue, bottom)
          } yield firstDoc % secondDoc
        case FailureG => None
        case Produce(producedValue) =>
          if (Objects.equals(producedValue, value)) Some(Empty)
          else None
        case WhiteSpace(width, height) => Some(WrappedSizedDocument(core.document.WhiteSpace(width, height)))
      }
    }

    helper(value, grammar).get
  }
}
