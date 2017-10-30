package core.bigrammar

import core.document.Empty
import core.grammar.{PrintGrammar, Produce}
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.GrammarKey
import core.responsiveDocument.ResponsiveDocument

object PrintBiGrammar {

  def toDocument(catalogue: GrammarCatalogue) = {
    val program = catalogue.find(ProgramGrammar)
    printReachableGrammars(program)
  }

  def printReachableGrammars(program: BiGrammar): ResponsiveDocument = {
    val reachableGrammars = getLabelled(program).collect({ case x: Labelled => x})
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

  def toDocument(grammar: BiGrammar) = toDocumentInner(removeProduceAndMap(contract(simplify(grammar))))

  private def toDocumentInner(grammar: BiGrammar): ResponsiveDocument = grammar match {
    case sequence: Sequence => withChoiceParenthesis(sequence.first) ~~ withChoiceParenthesis(sequence.second)
    case topBottom: TopBottom => withChoiceParenthesis(topBottom.first) ~~ "%" ~~withChoiceParenthesis(topBottom.second)
    case choice:Choice => toDocumentInner(choice.left) ~~ "|" ~~ toDocumentInner(choice.right)
    case many:ManyHorizontal => withParenthesis(many.inner) ~ "*"
    case many:ManyVertical => withParenthesis(many.inner) ~ "%*"
    case OptionGrammar(inner, _) => withParenthesis(inner) ~ "?"
    case RegexG(value) => s"Regex($value)"
    case keyword: Keyword => keyword.value
    case delimiter: Delimiter => delimiter.value
    case ValueGrammar(value) => value.toString
    case BiFailure(message) => message
    case fromString:FromStringGrammar => PrintGrammar.toDocument(fromString.grammar)
    case labelled: Labelled => grammarKeyToName(labelled.name)
    case StringLiteral => "string"
    case As(inner, key) => withParenthesis(inner) ~ s".As($key)"
    case print: Print => Empty //("print(": ResponsiveDocument) ~ print.document ~ ")"
    case ignore: IgnoreLeft =>
      val sequenceLike = ignore.inner.asInstanceOf[SequenceLike]
      toDocumentInner(sequenceLike.first) ~ "~>" ~ toDocumentInner(sequenceLike.second)
    case ignore: IgnoreRight =>
      val sequenceLike = ignore.inner.asInstanceOf[SequenceLike]
      toDocumentInner(sequenceLike.first) ~ "~<" ~ toDocumentInner(sequenceLike.second)
    case map: MapGrammar => toDocumentInner(map.inner) //("Map": ResponsiveDocument) ~ toDocumentInner(map.inner).inParenthesis
    case ParseWhiteSpace => ""
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
//    key match {
//    case key: KeyGrammar => key.toString
//    case _ =>
//      val regex = new Regex("Grammar\\$")
//      regex.replaceAllIn(key.getClass.getSimpleName, "")
//  }

  trait FakeBiGrammar extends BiGrammar {

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = ???

    override def children: Seq[BiGrammar] = ???
  }

  case class OptionGrammar(inner: BiGrammar, value: Any) extends FakeBiGrammar

  def simplify(grammar: BiGrammar): BiGrammar = grammar.map {
    case choice: Choice =>
      val left = choice.left
      val right = choice.right
      if (left.isInstanceOf[BiFailure])
        return right

      if (right.isInstanceOf[BiFailure])
        return left

      choice
    case sequence: SequenceLike =>
      val left = sequence.first
      val right = sequence.second
      if (left.isInstanceOf[BiFailure])
        return left

      if (right.isInstanceOf[BiFailure])
        return right

      sequence
    case x => x
  }

  def contract(grammar: BiGrammar): BiGrammar = grammar.map {
    case choice: Choice =>
      val left = choice.left
      val right = choice.right

      right match {
        case produce: Produce =>
          return OptionGrammar(left, produce.result)
        case _ =>
      }

      left match {
        case produce: Produce =>
          return OptionGrammar(right, produce.result)
        case _ =>
      }

      choice
    case x => x
  }

  def removeProduceAndMap(grammar: BiGrammar): BiGrammar = grammar.map {
    case sequence: SequenceLike =>
      val left = sequence.first
      val right = sequence.second
      if (left.isInstanceOf[Produce])
        return right

      if (right.isInstanceOf[Produce])
        return left

      sequence
    case many: Many if many.inner.isInstanceOf[Produce] => many.inner
    case map: MapGrammar => map.inner
    case x => x
  }

  def getLabelled(grammar: BiGrammar): Seq[Labelled] = {
    new RootGrammar(grammar).selfAndDescendants.flatMap((p: GrammarPath) => p.get match { case x: Labelled => Some(x); case _ => None} )
  }
}
