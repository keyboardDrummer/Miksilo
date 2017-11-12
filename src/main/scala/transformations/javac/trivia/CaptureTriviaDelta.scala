package transformations.javac.trivia

import core.bigrammar.BiGrammarToGrammar.{Result, StateM}
import core.bigrammar.grammars.CustomGrammar
import core.bigrammar.printer.TryState.{NodePrinter, State}
import core.bigrammar.{BiGrammar, BiGrammarToGrammar, WithMapG}
import core.grammar.Grammar
import core.particles.grammars.LanguageGrammars
import core.particles.{DeltaWithGrammar, Language, NodeGrammar}
import core.responsiveDocument.ResponsiveDocument
import JavaStyleCommentsC.CommentCounter

import scala.util.Try

object CaptureTriviaDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
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

  case class NodeCounterReset(var node: BiGrammar) extends CustomGrammar {

    override def children = Seq(node)

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      children.head ^^ { untyped =>
        val result = untyped.asInstanceOf[Result]
        StateM((state: BiGrammarToGrammar.State) => {
          val newState = state + (CommentCounter -> 0)
          result.run(newState)
        })
      }
    }

    override def createPrinter(recursive: (BiGrammar) => NodePrinter): NodePrinter = {
      val nodePrinter: NodePrinter = recursive(node)
      new NodePrinter {
        override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {
          val newState = state + (CommentCounter -> 0)
          nodePrinter.write(from, newState)
        }
      }
    }

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = NodeCounterReset(newChildren.head)

    override def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument = toDocumentInner(node)

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(node)
  }

  override def description: String = "Causes trivia to be captured in the AST during parsing"
}
