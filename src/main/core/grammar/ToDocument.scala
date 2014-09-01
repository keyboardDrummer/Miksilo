package core.grammar

import core.grammarDocument.ToGrammar
import core.responsiveDocument.ResponsiveDocument
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}

import scala.collection.immutable.Stream.Cons
import scala.util.matching.Regex

object ToDocument {

  def toDocument(catalogue: GrammarCatalogue) = {
    val program = ToGrammar.toGrammar(catalogue.find(ProgramGrammar))
    val reachableGrammars = getLabelled(program).collect({ case x: Labelled => x})
    val document = reachableGrammars.map(grammar => toTopLevelDocument(grammar)).reduce((a, b) => a %% b)
    document
  }

  def toTopLevelDocument(labelled: Labelled): ResponsiveDocument = {

    def getOrs(grammar: Grammar): Seq[Grammar] = grammar match {
      case Choice(left, right) => getOrs(left) ++ getOrs(right)
      case _ => Seq(grammar)
    }

    val transformed: Grammar = transform(labelled.inner)
    val ors: Seq[Grammar] = getOrs(transformed)
    val result = toDocument(labelled) ~~ "=>" ~~ ors.map(or => toDocument(or)).reduce((a, b) => a % b)
    result
  }

  def toDocument(grammar: Grammar): ResponsiveDocument = grammar match {
    case Sequence(left, right) =>
      def withParenthesis(grammar: Grammar): ResponsiveDocument = grammar match {
        case choice: Choice => toDocument(choice).inParenthesis
        case _ => toDocument(grammar)
      }
      withParenthesis(left) ~~ withParenthesis(right)
    case Choice(left, right) => toDocument(left) ~~ "|" ~~ toDocument(right)
    case Many(inner: Labelled) => toDocument(inner) ~ "*"
    case Many(inner) => toDocument(inner).inParenthesis ~ "*"
    case Keyword(value) => ResponsiveDocument.text(value)
    case Option(inner: Labelled) => toDocument(inner) ~ "?"
    case Option(inner) => toDocument(inner).inParenthesis ~ "?"
    case RegexG(value) => s"Regex($value)"
    case Produce(value) => "produce"
    case FailureG => "fail"
    case NumberG => "number"
    case Identifier => "identifier"
    case labelled: Labelled =>
      val key: AnyRef = labelled.name
      grammarKeyToName(key)
  }

  def grammarKeyToName(key: Any): String = {
    val regex = new Regex("Grammar\\$")
    regex.replaceAllIn(key.getClass.getSimpleName, "")
  }

  case class Option(inner: Grammar) extends Grammar

  def transform(grammar: Grammar): Grammar = grammar.simplify match {
    case Choice(_left, _right) =>
      val left = transform(_left)
      val right = transform(_right)
      if (left == FailureG)
        return right

      if (right == FailureG)
        return left

      if (right.isInstanceOf[Produce]) {
        return Option(left)
      }

      Choice(left, right)
    case map: MapGrammar => transform(map.inner)
    case Sequence(_left, _right) =>
      val left = transform(_left)
      val right = transform(_right)
      if (left.isInstanceOf[Produce])
        return right

      if (right.isInstanceOf[Produce])
        return left

      Sequence(left, right)
    case Many(inner) => Many(transform(inner))
    case Delimiter(value) => Keyword(value)
    case _ => grammar
  }

  def getLabelled(grammar: Grammar): Stream[Labelled] = {
    grammar.fold[Stream[Labelled]](Stream.empty, (inner, grammar) => grammar.simplify match {
      case choice: Choice => inner(choice.left) ++ inner(choice.right)
      case sequence: Sequence => inner(sequence.first) ++ inner(sequence.second)
      case many: Many => inner(many.inner)
      case labelled: Labelled => new Cons(labelled, inner(labelled.inner))
      case map: MapGrammar => inner(map.inner)
      case x => Stream.empty
    })
  }
}
