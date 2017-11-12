package transformations.javac.trivia

import core.bigrammar.BiGrammarToGrammar.{Result, StateM}
import core.bigrammar._
import core.bigrammar.grammars._
import core.bigrammar.printer.TryState.{NodePrinter, State}
import core.grammar.Grammar
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Key, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try

object JavaStyleCommentsC extends DeltaWithGrammar {

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.trivia.inner = new Sequence(CommentsGrammar, ParseWhiteSpace).ignoreRight
  }

  object CommentCounter extends Key
  case class Comment(index: Int) extends NodeField

  object CommentsGrammar extends CustomGrammar with BiGrammarWithoutChildren {

    val commentGrammar = getCommentGrammar
    val comments = new ManyVertical(new Sequence(ParseWhiteSpace, commentGrammar).ignoreLeft)

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      recursive(comments) ^^ { untyped =>
        val result = untyped.asInstanceOf[Result]
        StateM((state: BiGrammarToGrammar.State) => {
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
