package deltas.trivia

import core.bigrammar.{BiGrammar, WithMap}
import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToParser.{CanMerge, Input}
import core.bigrammar.grammars.CustomGrammar
import core.bigrammar.printer.TryState
import core.deltas.grammars.{LanguageGrammars, TriviasGrammar}
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.{Key, NodeField, NodeGrammar}
import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

object StoreTriviaDelta extends DeltaWithGrammar {

  override def description: String = "Causes trivia to be captured in the AST during parsing"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    resetCounterWhenEnteringNode(grammars)

    val triviasGrammar = grammars.find(TriviasGrammar)
    if (!triviasGrammar.inner.isInstanceOf[StoreTrivia]) //This check enables us to apply this delta multiple times.
      triviasGrammar.inner = new StoreTrivia(triviasGrammar.inner)
  }

  private def resetCounterWhenEnteringNode(grammars: LanguageGrammars): Unit = {
    import grammars._

    var visited = Set.empty[BiGrammar]
    for (path <- root.descendants) {
      if (!visited.contains(path.value)) { //TODO make sure the visited check isn't needed.
        visited += path.value
        path.value match {
          case node: NodeGrammar =>
            if (!path.parent.isInstanceOf[NodeCounterReset]) {
              path.set(NodeCounterReset(node))
            }
          case _ =>
        }
      }
    }
  }

  object TriviaCounter extends Key
  class ParseTriviaField(val input: Input) extends NodeField with CanMerge {
    override lazy val toString = s"Trivia,${input.position.offset}"
    override def merge(first: Any, second: Any) = first.asInstanceOf[Seq[_]] ++ second.asInstanceOf[Seq[_]]
  }

  case class Trivia(index: Int) extends NodeField {
    override lazy val toString = s"Trivia$index"
  }

  class StoreTrivia(var triviaGrammar: BiGrammar) extends CustomGrammar with BiGrammar {

    def getFieldAndIncrementCounter: TryState[NodeField] = (state: State) => {
      val counter: Int = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
      val field = Trivia(counter)
      val newState = state + (TriviaCounter -> (counter + 1))
      scala.util.Success(newState, field)
    }

    override def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result] = {
      val triviaParser = recursive(triviaGrammar)
      leftRightSimple[Input, Result, Result](PositionParser, triviaParser, (position, triviasWithMap) => {
        val trivias = triviasWithMap.value.asInstanceOf[Seq[_]]
        val field = new ParseTriviaField(position)
        WithMap[Any]((), Map(field -> Seq(trivias)))
      })
    }

    override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new NodePrinter {
      val triviaPrinter: NodePrinter = recursive(triviaGrammar)

      override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = for {
        key <- getFieldAndIncrementCounter
        value = from.namedValues.getOrElse(key, Seq.empty)
        result <- triviaPrinter.write(WithMap[Any](value, from.namedValues))
      } yield result
    }

    override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = "StoreTrivias"

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

    override def children: Seq[BiGrammar] = Seq(triviaGrammar)

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = new StoreTrivia(newChildren.head)
  }

  case class NodeCounterReset(var node: NodeGrammar) extends CustomGrammar {

    override def children = Seq(node)

    def resetAndRestoreCounter[T](inner: TryState[T]): TryState[T] = state => {
      val initialCounter = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
      val newState = state + (TriviaCounter -> 0)
      inner.run(newState).map(p => {
        val (resultState, value) = p
        (resultState + (TriviaCounter -> initialCounter), value)
      })
    }

    // TODO move this transformation to the printing side.
    override def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result] = {
      val oldInner = recursive(node.inner)
      val newInner: Parser[Result] = oldInner.map((result: Result) => {
        var trivias: Seq[(ParseTriviaField, Any)] = List.empty
        var rest: List[(Any, Any)] = List.empty
        for(namedValue <- result.namedValues) {
          namedValue._1 match {
            case parseTriviaField: ParseTriviaField => trivias = namedValue._2.asInstanceOf[Seq[_]].map(value => (parseTriviaField, value)) ++ trivias
            case _ => rest ::= namedValue
          }
        }

        val fixedTrivias = trivias.sortBy(t => t._1.input.position.offset).
          zipWithIndex.map(withIndex => (Trivia(withIndex._2), withIndex._1._2)).
          filter(v => v._2.asInstanceOf[Seq[_]].nonEmpty)
        WithMap(result.value, (rest ++ fixedTrivias).toMap)
      })
      newInner.map(input => NodeGrammar.construct(input, node.shape))
    }

    override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = {
      val nodePrinter: NodePrinter = recursive(node)
      from: WithMap[Any] => {
        resetAndRestoreCounter(nodePrinter.write(from))
      }
    }

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = NodeCounterReset(newChildren.head.asInstanceOf[NodeGrammar])

    override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = toDocumentInner(node)

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(node)
  }

  override def dependencies: Set[Contract] = Set.empty
}
