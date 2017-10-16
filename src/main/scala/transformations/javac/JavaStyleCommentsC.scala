package transformations.javac

import core.bigrammar.BiGrammarToGrammar.{Result, StateM}
import core.bigrammar._
import core.bigrammar.printer.TryState.{NodePrinter, State}
import core.grammar.Grammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, NodeField}
import core.particles.{DeltaWithGrammar, Language}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try
import scala.util.matching.Regex

object JavaStyleCommentsC extends DeltaWithGrammar {

  object CommentGrammar
//  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
//    val commentsGrammar = grammars.create(CommentGrammar, getCommentsGrammar)
//
//    for(path <- grammars.root.selfAndDescendants.
//      filter(path => path.get.children.isEmpty))
//    {
//      addCommentPrefixToGrammar(commentsGrammar, path.asInstanceOf[GrammarReference])
//    }
//  }
//
//  def addCommentPrefixToGrammar(commentsGrammar: BiGrammar, leafReference: GrammarReference): Unit = {
//    leafReference.set(commentsGrammar ~> leafReference.get)
//  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val commentsGrammar = grammars.create(CommentGrammar, getCommentsGrammar)

    for(path <- grammars.root.selfAndDescendants.
      filter(path => path.get.isInstanceOf[Layout]))
    {
      addCommentPrefixToGrammar(commentsGrammar, path.asInstanceOf[GrammarReference])
    }
    val node = grammars.root.find(p => p.get.isInstanceOf[NodeGrammar]).get.get.asInstanceOf[NodeGrammar]
    //node.inner = commentsGrammar ~> node.inner
  }

  def addCommentPrefixToGrammar(commentsGrammar: BiGrammar, layoutReference: GrammarReference): Unit = {
    layoutReference.get match {
      case sequence: SequenceLike => sequence.second =
        if (sequence.horizontal) commentsGrammar ~> sequence.second
        else commentsGrammar %> sequence.second
      case many: ManyVertical => layoutReference.set(many.inner.manySeparatedVertical(commentsGrammar))
      case many: ManyHorizontal => layoutReference.set(many.inner.manySeparated(commentsGrammar))
    }
  }

  object CommentCounter extends Key
  case class Comment(index: Int) extends NodeField
  def getCommentsGrammar: BiGrammar = {
    val commentGrammar = getCommentGrammar
    val comments = commentGrammar.manyVertical
    new SuperCustomGrammar {

      override def createGrammar(recursive: (BiGrammar) => Grammar) = {
        val commentsGrammar = recursive(comments)
        commentsGrammar ^^ {
          case result: Result => StateM((state: BiGrammarToGrammar.State) => {
            val counter: Int = state.getOrElse(CommentCounter, 0).asInstanceOf[Int]
            val key = Comment(counter)
            var newState = state + (CommentCounter -> (counter + 1))
            val inner = result.run(state)
            val innerValue = inner._2.value.asInstanceOf[Seq[_]]
            val map: Map[Any,Any] = if (innerValue.nonEmpty) Map(key -> innerValue) else Map.empty
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
            case Some(comment) => comment.asInstanceOf[Seq[String]]
            case _ => Seq.empty
          }
          commentsPrinter.write(WithMapG(value, from.map), newState)
        }
      }
    }
  }

  def getCommentGrammar: BiGrammar = {
    val regex: Regex = new Regex( """/\*.*\*/""")
    RegexG(regex) ~ space
  }

  override def description: String = "Adds Java-style comments to the language"
}
