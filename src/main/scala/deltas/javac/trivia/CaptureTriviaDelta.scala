package deltas.javac.trivia

import core.bigrammar.BiGrammarToGrammar.{Result, StateM}
import core.bigrammar.grammars._
import core.bigrammar.printer.TryState.{NodePrinter, State}
import core.bigrammar.{BiGrammar, BiGrammarToGrammar, WithMapG}
import core.grammar.Grammar
import core.deltas.grammars.{LanguageGrammars, TriviasGrammar}
import core.deltas.{DeltaWithGrammar, Language, NodeGrammar}
import core.responsiveDocument.ResponsiveDocument
import core.deltas.node.{Key, NodeField}

import scala.util.Try

object CaptureTriviaDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    resetCounterWhenEnteringNode(grammars)

    val triviasGrammar = grammars.find(TriviasGrammar)
    if (!triviasGrammar.inner.isInstanceOf[StoreTrivias])
      triviasGrammar.inner = new StoreTrivias(triviasGrammar.inner)
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
  case class Trivias(index: Int) extends NodeField

  class StoreTrivias(triviasGrammar: BiGrammar) extends CustomGrammar with BiGrammarWithoutChildren {

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      recursive(triviasGrammar) ^^ { untyped =>
        val result = untyped.asInstanceOf[Result]
        StateM((state: BiGrammarToGrammar.State) => {
          val counter: Int = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
          val key = Trivias(counter)
          val newState = state + (TriviaCounter -> (counter + 1))
          val inner = result.run(state)
          val innerValue = inner._2.value.asInstanceOf[Seq[_]]
          val map: Map[Any,Any] =
            if (innerValue.nonEmpty)
              Map(key -> innerValue)
            else Map.empty
          (newState, WithMapG[Any](Unit, map))
        })
      }
    }

    override def createPrinter(recursive: (BiGrammar) => NodePrinter) = new NodePrinter {

      val triviasPrinter: NodePrinter = recursive(triviasGrammar)
      override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {
        val counter: Int = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
        val key = Trivias(counter)
        val newState = state + (TriviaCounter -> (counter + 1))
        val value = from.map.get(key) match {
          case Some(trivia) =>
            trivia.asInstanceOf[Seq[String]]
          case _ => Seq.empty
        }
        triviasPrinter.write(WithMapG(value, from.map), newState)
      }
    }

    override def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument = "StoreTrivias"

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
  }

  case class NodeCounterReset(var node: BiGrammar) extends CustomGrammar {

    override def children = Seq(node)

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      children.head ^^ { untyped =>
        val result = untyped.asInstanceOf[Result]
        StateM((state: BiGrammarToGrammar.State) => {
          val initialCounter = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
          val newState = state + (TriviaCounter -> 0)
          val (resultState, value) = result.run(newState)
          (resultState + (TriviaCounter -> initialCounter), value)
        })
      }
    }

    override def createPrinter(recursive: (BiGrammar) => NodePrinter): NodePrinter = {
      val nodePrinter: NodePrinter = recursive(node)
      new NodePrinter {
        override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {
          val initialCounter = state.getOrElse(TriviaCounter, 0).asInstanceOf[Int]
          val newState = state + (TriviaCounter -> 0)
          nodePrinter.write(from, newState).map({
            case (resultState, value) => (resultState + (TriviaCounter -> initialCounter), value)
          })
        }
      }
    }

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = NodeCounterReset(newChildren.head)

    override def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument = toDocumentInner(node)

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(node)
  }

  override def description: String = "Causes trivia to be captured in the AST during parsing"
}
