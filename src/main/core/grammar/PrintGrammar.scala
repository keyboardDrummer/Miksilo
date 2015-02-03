package core.grammar

import core.grammarDocument.BiGrammarToGrammar
import core.responsiveDocument.ResponsiveDocument
import core.transformation.grammars.{GrammarCatalogue, ProgramGrammar}

import scala.collection.immutable.Stream.Cons
import scala.util.matching.Regex

object PrintGrammar {

  def toDocument(catalogue: GrammarCatalogue) = {
    val program = BiGrammarToGrammar.toGrammar(catalogue.find(ProgramGrammar))
    printReachableGrammars(program)
  }

  def printReachableGrammars(program: Grammar): ResponsiveDocument = {
    val reachableGrammars = getLabelled(program).collect({ case x: Labelled => x})
    reachableGrammars.map(grammar => toTopLevelDocument(grammar)).reduce((a, b) => a %% b)
  }

  def toTopLevelDocument(labelled: Labelled): ResponsiveDocument = {

    def getOrs(grammar: Grammar): Seq[Grammar] = grammar match {
      case Choice(left, right) => getOrs(left) ++ getOrs(right)
      case _ => Seq(grammar)
    }

    val transformed: Grammar = transform(labelled.inner)
    val ors: Seq[Grammar] = getOrs(transformed)
    val result = toDocumentInner(labelled) ~~ "=>" ~~ ors.map(or => toDocumentInner(or)).reduce((a, b) => a % b)
    result
  }

  def toDocument(grammar: Grammar) = toDocumentInner(transform(grammar))

  private def toDocumentInner(grammar: Grammar): ResponsiveDocument = grammar match {
    case Sequence(left, right) =>
      def withParenthesis(grammar: Grammar): ResponsiveDocument = grammar match {
        case choice: Choice => toDocumentInner(choice).inParenthesis
        case _ => toDocumentInner(grammar)
      }
      withParenthesis(left) ~~ withParenthesis(right)
    case Choice(left, right) => toDocumentInner(left) ~~ "|" ~~ toDocumentInner(right)
    case Many(inner: Labelled) => toDocumentInner(inner) ~ "*"
    case Many(inner) => toDocumentInner(inner).inParenthesis ~ "*"
    case Keyword(value) => ResponsiveDocument.text(value)
    case Option(inner: Labelled) => toDocumentInner(inner) ~ "?"
    case Option(inner) => toDocumentInner(inner).inParenthesis ~ "?"
    case RegexG(value) => s"Regex($value)"
    case Produce(value) => "produce"
    case FailureG => "fail"
    case NumberG => "number"
    case Identifier => "identifier"
    case labelled: Labelled =>
      val key: AnyRef = labelled.name
      grammarKeyToName(key)
    case StringLiteral => "string"
  }

  def grammarKeyToName(key: Any): String = key match {
    case string: String => string
    case _ => {
      val regex = new Regex("Grammar\\$")
      regex.replaceAllIn(key.getClass.getSimpleName, "")
    }
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
