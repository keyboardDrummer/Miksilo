package deltas.javac.trivia

import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.grammars._
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.printer.TryState
import core.bigrammar.{BiGrammar, BiGrammarToParser, StateFull, WithMap}
import core.deltas.grammars.{LanguageGrammars, TriviasGrammar}
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.{Key, NodeField, NodeGrammar}
import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._

object StoreTriviaDelta extends DeltaWithGrammar {

  override def description: String = "Causes trivia to be captured in the AST during parsing"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    resetCounterWhenEnteringNode(grammars)

    val triviasGrammar = grammars.find(TriviasGrammar)
    if (!triviasGrammar.inner.isInstanceOf[StoreTrivia]) //This check enables us to apply this delta multiple times.
      triviasGrammar.inner = new StoreTrivia(triviasGrammar.inner)
  }

  private def resetCounterWhenEnteringNode(grammars: LanguageGrammars): Unit = {
    var visited = Set.empty[BiGrammar]
    for (path <- grammars.root.descendants) {
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
  case class Trivia(index: Int) extends NodeField

  class StoreTrivia(var triviaGrammar: BiGrammar) extends CustomGrammar with BiGrammar {

    def getFieldAndIncrementCounter: StateFull[NodeField] = (state: State) => {
      val counter: Int = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
      val field = Trivia(counter)
      val newState = state + (TriviaCounter -> (counter + 1))
      (newState, field)
    }

    override def toParser(recursive: BiGrammar => EditorParser[Result]): EditorParser[Result] = {
      val triviaParser = recursive(triviaGrammar)
      triviaParser.map(statefulTrivias =>
        for {
          field <- getFieldAndIncrementCounter
          triviasWithMap <- statefulTrivias
        } yield {
          val trivias = triviasWithMap.value.asInstanceOf[Seq[_]]
          WithMap[Any](Unit, if (trivias.nonEmpty) Map(field -> trivias) else Map.empty)
        }
      )
    }

    override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new NodePrinter {
      val triviaPrinter: NodePrinter = recursive(triviaGrammar)

      override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = for {
        key <- TryState.fromStateM(getFieldAndIncrementCounter)
        value = from.namedValues.getOrElse(key, Seq.empty)
        result <- triviaPrinter.write(WithMap[Any](value, from.namedValues))
      } yield result
    }

    override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = "StoreTrivias"

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

    override def children: Seq[BiGrammar] = Seq(triviaGrammar)

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = new StoreTrivia(newChildren.head)
  }

  case class NodeCounterReset(var node: BiGrammar) extends CustomGrammar {

    override def children = Seq(node)

    def resetAndRestoreCounter[T](inner: TryState[T]): TryState[T] = state => {
      val initialCounter = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
      val newState = state + (TriviaCounter -> 0)
      inner.run(newState).map(p => {
        val (resultState, value) = p
        (resultState + (TriviaCounter -> initialCounter), value)
      })
    }

    def resetAndRestoreCounter[T](inner: StateFull[T]): StateFull[T] = state => {
      resetAndRestoreCounter(TryState.fromStateM(inner)).run(state).get
    }

    override def toParser(recursive: BiGrammar => EditorParser[Result]): EditorParser[Result] = {
      recursive(node).map(result => resetAndRestoreCounter(result))
    }

    override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = {
      val nodePrinter: NodePrinter = recursive(node)
      from: WithMap[Any] => {
        resetAndRestoreCounter(nodePrinter.write(from))
      }
    }

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = NodeCounterReset(newChildren.head)

    override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = toDocumentInner(node)

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(node)
  }

  override def dependencies: Set[Contract] = Set.empty
}
