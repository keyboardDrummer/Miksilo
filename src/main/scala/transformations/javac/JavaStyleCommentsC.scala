package transformations.javac

import core.bigrammar.BiGrammarToGrammar.{Result, StateM}
import core.bigrammar._
import core.bigrammar.printer.TryState.{NodePrinter, State}
import core.grammar.Grammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, NodeField}
import core.particles.{DeltaWithGrammar, Language, NodeGrammar}
import core.responsiveDocument.ResponsiveDocument
import transformations.javac.JavaStyleCommentsC.CommentCounter

import scala.util.Try

object CaptureTriviaDelta extends DeltaWithGrammar {
  override def transformGrammars(grammars: GrammarCatalogue, language: Language): Unit = {
    var visited = Set.empty[BiGrammar]
    for (path <- language.root.descendants) {
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

  case class NodeCounterReset(var node: BiGrammar) extends SuperCustomGrammar {

    override def children = Seq(node)

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      children.head ^^ {
        case result: Result => StateM((state: BiGrammarToGrammar.State) => {
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

object JavaStyleCommentsC extends DeltaWithGrammar {

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    grammars.trivia.inner = new Sequence(CommentsGrammar, ParseWhiteSpace).ignoreRight
  }

  object CommentCounter extends Key
  case class Comment(index: Int) extends NodeField

  object CommentsGrammar extends SuperCustomGrammar with BiGrammarWithoutChildren {

    val commentGrammar = getCommentGrammar
    val comments = new ManyVertical(new Sequence(ParseWhiteSpace, commentGrammar).ignoreLeft)

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      recursive(comments) ^^ {
        case result: Result => StateM((state: BiGrammarToGrammar.State) => {
          val counter: Int = state.getOrElse(CommentCounter, 0).asInstanceOf[Int]
          val key = Comment(counter)
          val newState = state + (CommentCounter -> (counter + 1))
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

      val commentsPrinter: NodePrinter = recursive(comments)
      override def write(from: WithMapG[Any], state: State): Try[(State, ResponsiveDocument)] = {
        val counter: Int = state.getOrElse(CommentCounter, 0).asInstanceOf[Int]
        val key = Comment(counter)
        val newState = state + (CommentCounter -> (counter + 1))
        val value = from.map.get(key) match {
          case Some(comment) =>
            comment.asInstanceOf[Seq[String]]
          case _ => Seq.empty
        }
        commentsPrinter.write(WithMapG(value, from.map), newState)
      }
    }

    override def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument = "Comments"

    override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
  }

  def getCommentGrammar: BiGrammar = {
    new Sequence(RegexG("""(?s)/\*.*?\*/""".r), space).ignoreRight
  }

  override def description: String = "Adds Java-style comments to the language"

}
