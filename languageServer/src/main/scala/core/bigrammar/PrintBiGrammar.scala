package core.bigrammar

import core.bigrammar.grammars._
import core.document.Empty
import core.language.node.GrammarKey
import core.responsiveDocument.ResponsiveDocument

object PrintBiGrammar {

  def printReachableGrammars(program: BiGrammar[_]): ResponsiveDocument = {
    val reachableGrammars = getLabelled(program)
    reachableGrammars.map(grammar => toTopLevelDocument(grammar)).reduce((a, b) => a %% b)
  }

  def toTopLevelDocument(labelled: Labelled[_]): ResponsiveDocument = {

    def getOrs(grammar: BiGrammar[_]): Seq[BiGrammar[_]] = grammar match {
      case choice: BiChoice[_] => getOrs(choice.left) ++ getOrs(choice.right)
      case _ => Seq(grammar)
    }

    val transformed: BiGrammar[_] = removeProduceAndMap(contract(simplify(labelled.inner)))
    val ors: Seq[BiGrammar[_]] = getOrs(transformed)
    val result = toDocumentInner(labelled) ~~ "=>" ~~ ors.map(or => toDocumentInner(or)).reduce((a, b) => a % b)
    result
  }

  def toDocument(grammar: BiGrammar[_]): ResponsiveDocument = toDocumentInner(removeProduceAndMap(contract(simplify(grammar))))

  private def toDocumentInner(grammar: BiGrammar[_]): ResponsiveDocument = grammar match {
    case sequence: BiSequence[_,_,_] =>
      val first = withChoiceParenthesis(sequence.first)
      val second = withChoiceParenthesis(sequence.second)
      if (sequence.horizontal) first ~~ second
      else first ~~ "%" ~~ second
    case choice:BiChoice[_] => toDocumentInner(choice.left) ~~ "|" ~~ toDocumentInner(choice.right)
    case many: ManyHorizontal[_] => withParenthesis(many.inner) ~ "*"
    case many:ManyVertical[_] => withParenthesis(many.inner) ~ "%*"
    case OptionGrammar(inner, _) => withParenthesis(inner) ~ "?"
    case regex: RegexGrammar => s"Regex(${regex.regex})"
    case keyword: Keyword => keyword.value
    case _: Identifier => "Identifier"
    case delimiter: Delimiter => delimiter.value
    case ValueGrammar(value) => if (value == null) "null" else value.toString
    case BiFailure(message) => message
    case labelled: Labelled[_] => grammarKeyToName(labelled.name)
    case NumberGrammar => "number"
    case StringLiteral => "string"
    case print: Print => Empty //("print(": ResponsiveDocument) ~ print.document ~ ")"
    case map: MapGrammar[_, _] => toDocumentInner(map.inner) //("Map": ResponsiveDocument) ~ toDocumentInner(map.inner).inParenthesis
    case ParseWhiteSpace => ""
    case custom: CustomGrammar[_] => custom.print(toDocumentInner)
    case _ => grammar.getClass.toString
  }

  private def withChoiceParenthesis(grammar: BiGrammar[_]): ResponsiveDocument = grammar match {
    case choice: BiChoice[_] => toDocumentInner(choice).inParenthesis
    case _ => toDocumentInner(grammar)
  }

  def withParenthesis(grammar: BiGrammar[_]): ResponsiveDocument = grammar match {
    case labelled:Labelled[_] => toDocumentInner(labelled)
    case x => toDocumentInner(x).inParenthesis
  }

  def grammarKeyToName(key: GrammarKey): String = key.toString

  trait FakeBiGrammar[Value] extends BiGrammar[Value] {
    override def withChildren(newChildren: Seq[BiGrammar[_]]): BiGrammar[Value] = ???
    override def children: Seq[BiGrammar[_]] = ???
    override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = true
  }

  case class OptionGrammar[Value](inner: BiGrammar[Value], value: Value) extends FakeBiGrammar[Value]

  def simplify[Value](grammar: BiGrammar[Value]): BiGrammar[Value] = {
    def map(grammar: BiGrammar[_]): BiGrammar[_] = grammar match {
      case choice: BiChoice[_] =>
        val left = choice.left
        val right = choice.right
        if (left.isInstanceOf[BiFailure[_]])
          return right

        if (right.isInstanceOf[BiFailure[_]])
          return left

        choice
      case sequence: BiSequence[_, _, _] =>
        val left = sequence.first
        val right = sequence.second
        if (left.isInstanceOf[BiFailure[_]])
          return left

        if (right.isInstanceOf[BiFailure[_]])
          return right

        sequence
      case x => x
    }
    grammar.deepMap(map).asInstanceOf[BiGrammar[Value]]
  }

  def contract(grammar: BiGrammar[_]): BiGrammar[_] = grammar.deepMap {
    case choice: BiChoice[_] =>
      val left = choice.left
      val right = choice.right

      right match {
        case produce: ValueGrammar[_] =>
          return OptionGrammar(left, produce.value)
        case _ =>
      }

      left match {
        case produce: ValueGrammar[_] =>
          return OptionGrammar(right, produce.value)
        case _ =>
      }

      choice
    case x => x
  }

  def removeProduceAndMap(grammar: BiGrammar[_]): BiGrammar[_] = grammar.deepMap {
    case sequence: BiSequence[_,_,_] =>
      val left = sequence.first
      val right = sequence.second
      if (left.isInstanceOf[ValueGrammar[_]])
        return right

      if (right.isInstanceOf[ValueGrammar[_]])
        return left

      sequence
    case many: Many[_] if many.inner.isInstanceOf[ValueGrammar[_]] => many.inner
    case map: MapGrammar[_, _] => map.inner
    case x => x
  }

  def getLabelled(grammar: BiGrammar[_]): Seq[Labelled[_]] = {
    grammar.selfAndDescendants.collect({ case x: Labelled[_] => x})
  }
}
