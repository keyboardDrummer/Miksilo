package core.bigrammar

import core.bigrammar.grammars._
import core.deltas.node.GrammarKey
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

object PrintBiGrammar {

  def printReachableGrammars(program: BiGrammar): ResponsiveDocument = {
    val reachableGrammars = getLabelled(program)
    reachableGrammars.map(grammar => toTopLevelDocument(grammar)).reduce((a, b) => a %% b)
  }

  def toTopLevelDocument(labelled: Labelled): ResponsiveDocument = {

    def getOrs(grammar: BiGrammar): Seq[BiGrammar] = grammar match {
      case choice:Choice => getOrs(choice.left) ++ getOrs(choice.right)
      case _ => Seq(grammar)
    }

    val transformed: BiGrammar = removeProduceAndMap(contract(simplify(labelled.inner)))
    val ors: Seq[BiGrammar] = getOrs(transformed)
    val result = toDocumentInner(labelled) ~~ "=>" ~~ ors.map(or => toDocumentInner(or)).reduce((a, b) => a % b)
    result
  }

  def toDocument(grammar: BiGrammar): ResponsiveDocument = toDocumentInner(removeProduceAndMap(contract(simplify(grammar))))

  private def toDocumentInner(grammar: BiGrammar): ResponsiveDocument = grammar match {
    case sequence: LeftRight => withChoiceParenthesis(sequence.first) ~~ withChoiceParenthesis(sequence.second)
    case topBottom: TopBottom => withChoiceParenthesis(topBottom.first) ~~ "%" ~~withChoiceParenthesis(topBottom.second)
    case choice:Choice => toDocumentInner(choice.left) ~~ "|" ~~ toDocumentInner(choice.right)
    case many:ManyHorizontal => withParenthesis(many.inner) ~ "*"
    case many:ManyVertical => withParenthesis(many.inner) ~ "%*"
    case OptionGrammar(inner, _) => withParenthesis(inner) ~ "?"
    case regex: RegexGrammar => s"Regex(${regex.regex})"
    case keyword: Keyword => keyword.value
    case delimiter: Delimiter => delimiter.value
    case ValueGrammar(value) => if (value == null) "null" else value.toString
    case BiFailure(message) => message
    case labelled: Labelled => grammarKeyToName(labelled.name)
    case NumberG => "number"
    case StringLiteral => "string"
    case As(inner, key) => withParenthesis(inner) ~ s".As($key)"
    case print: Print => Empty //("print(": ResponsiveDocument) ~ print.document ~ ")"
    case ignore: IgnoreLeft =>
      val sequenceLike = ignore.inner.asInstanceOf[Sequence]
      toDocumentInner(sequenceLike.first) ~ "~>" ~ toDocumentInner(sequenceLike.second)
    case ignore: IgnoreRight =>
      val sequenceLike = ignore.inner.asInstanceOf[Sequence]
      toDocumentInner(sequenceLike.first) ~ "~<" ~ toDocumentInner(sequenceLike.second)
    case map: MapGrammarWithMap => toDocumentInner(map.inner) //("Map": ResponsiveDocument) ~ toDocumentInner(map.inner).inParenthesis
    case ParseWhiteSpace => ""
    case custom: CustomGrammar => custom.print(toDocumentInner)
    case _ => grammar.getClass.toString
  }

  private def withChoiceParenthesis(grammar: BiGrammar): ResponsiveDocument = grammar match {
    case choice: Choice => toDocumentInner(choice).inParenthesis
    case _ => toDocumentInner(grammar)
  }

  private def withParenthesis(grammar: BiGrammar): ResponsiveDocument = grammar match {
    case labelled:Labelled => toDocumentInner(labelled)
    case x => toDocumentInner(x).inParenthesis
  }

  def grammarKeyToName(key: GrammarKey): String = key.toString

  trait FakeBiGrammar extends BiGrammar {
    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = ???
    override def children: Seq[BiGrammar] = ???
    override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
  }

  case class OptionGrammar(inner: BiGrammar, value: Any) extends FakeBiGrammar

  def simplify(grammar: BiGrammar): BiGrammar = grammar.deepMap {
    case choice: Choice =>
      val left = choice.left
      val right = choice.right
      if (left.isInstanceOf[BiFailure])
        return right

      if (right.isInstanceOf[BiFailure])
        return left

      choice
    case sequence: Sequence =>
      val left = sequence.first
      val right = sequence.second
      if (left.isInstanceOf[BiFailure])
        return left

      if (right.isInstanceOf[BiFailure])
        return right

      sequence
    case x => x
  }

  def contract(grammar: BiGrammar): BiGrammar = grammar.deepMap {
    case choice: Choice =>
      val left = choice.left
      val right = choice.right

      right match {
        case produce: ValueGrammar =>
          return OptionGrammar(left, produce.value)
        case _ =>
      }

      left match {
        case produce: ValueGrammar =>
          return OptionGrammar(right, produce.value)
        case _ =>
      }

      choice
    case x => x
  }

  def removeProduceAndMap(grammar: BiGrammar): BiGrammar = grammar.deepMap {
    case sequence: Sequence =>
      val left = sequence.first
      val right = sequence.second
      if (left.isInstanceOf[ValueGrammar])
        return right

      if (right.isInstanceOf[ValueGrammar])
        return left

      sequence
    case many: Many if many.inner.isInstanceOf[ValueGrammar] => many.inner
    case map: MapGrammarWithMap => map.inner
    case x => x
  }

  def getLabelled(grammar: BiGrammar): Seq[Labelled] = {
    grammar.selfAndDescendants.collect({ case x: Labelled => x})
  }
}
