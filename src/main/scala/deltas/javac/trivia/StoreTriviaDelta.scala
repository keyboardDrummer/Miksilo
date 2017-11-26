package deltas.javac.trivia

import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToGrammar.{Result, WithMap}
import core.bigrammar.grammars._
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.printer.TryState
import core.bigrammar.{BiGrammar, BiGrammarToGrammar, StateFull, WithMapG}
import core.deltas.grammars.{LanguageGrammars, TriviasGrammar}
import core.deltas.node.{Key, NodeField}
import core.deltas.{DeltaWithGrammar, Language, NodeGrammar}
import core.grammar.Grammar
import core.responsiveDocument.ResponsiveDocument
import core.deltas.grammars.{LanguageGrammars, TriviasGrammar}
import core.deltas.{DeltaWithGrammar, Language, NodeGrammar}
import core.responsiveDocument.ResponsiveDocument
import core.deltas.node.{Key, NodeField}

import scala.util.Try

object StoreTriviaDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    resetCounterWhenEnteringNode(grammars)

    val triviasGrammar = grammars.find(TriviasGrammar)
    if (!triviasGrammar.inner.isInstanceOf[StoreTrivia])
      triviasGrammar.inner = new StoreTrivia(triviasGrammar.inner)
  }

  private def resetCounterWhenEnteringNode(grammars: LanguageGrammars) = {
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

  class StoreTrivia(triviaGrammar: BiGrammar) extends CustomGrammar with BiGrammarWithoutChildren {

    def getKeyAndIncrementCounter: StateFull[NodeField] = (state: State) => {
      val counter: Int = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
      val key = Trivia(counter)
      val newState = state + (TriviaCounter -> (counter + 1))
      (newState, key)
    }

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      recursive(triviaGrammar) ^^ { untyped =>
        val result = untyped.asInstanceOf[StateFull[WithMap]]
        for {
          key <- getKeyAndIncrementCounter
          inner <- result
        } yield {
          val innerValue = inner.value.asInstanceOf[Seq[_]]
          WithMapG[Any](Unit, if (innerValue.nonEmpty) Map(key -> innerValue) else Map.empty)
        }
      }
    }

    override def createPrinter(recursive: BiGrammar => NodePrinter) = new NodePrinter {
      val triviaPrinter: NodePrinter = recursive(triviaGrammar)

      override def write(from: WithMapG[Any]): TryState[ResponsiveDocument] = for {
        key <- TryState.fromStateM(getKeyAndIncrementCounter)
        value = from.map.getOrElse(key, Seq.empty)
        result <- triviaPrinter.write(WithMapG[Any](value, from.map))
      } yield result
    }

    override def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument = "StoreTrivias"

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
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

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      children.head ^^ { untyped =>
        val result = untyped.asInstanceOf[Result]
        resetAndRestoreCounter(result)
      }
    }

    override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = {
      val nodePrinter: NodePrinter = recursive(node)
      (from: WithMapG[Any]) => {
        resetAndRestoreCounter(nodePrinter.write(from))
      }
    }

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = NodeCounterReset(newChildren.head)

    override def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument = toDocumentInner(node)

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(node)
  }

  override def description: String = "Causes trivia to be captured in the AST during parsing"
}
