package transformations.javac

import core.bigrammar._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, NodeField}
import core.particles.{DeltaWithGrammar, Language}

import scala.util.matching.Regex

object JavaStyleCommentsC extends DeltaWithGrammar {

  object CommentGrammar
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val commentsGrammar = grammars.create(CommentGrammar, getCommentsGrammar)

    for(path <- grammars.root.selfAndDescendants.
      filter(path => path.get.children.isEmpty))
    {
      addCommentPrefixToGrammar(commentsGrammar, path.asInstanceOf[GrammarReference])
    }
  }

  def addCommentPrefixToGrammar(commentsGrammar: BiGrammar, leafReference: GrammarReference): Unit = {
    leafReference.set(commentsGrammar ~> leafReference.get)
  }

  object CommentCounter extends Key
  case class Comment(index: Int) extends NodeField
  def getCommentsGrammar: BiGrammar = {
    val commentGrammar = getCommentGrammar
    val comments = commentGrammar.manyVertical
    new MapGrammar(comments, { case withMap: WithMapG[Any] =>
      val counter: Int = withMap.map.getOrElse(CommentCounter, 0).asInstanceOf[Int]
      val key = Comment(counter)
      var newState = withMap.map + (CommentCounter -> (counter + 1))
      if (withMap.value.asInstanceOf[Seq[_]].nonEmpty)
        newState += (key -> withMap.value)
      WithMapG(Unit, newState)
    }, {
      case withMap: WithMapG[Any] =>
        val counter: Int = withMap.map.getOrElse(CommentCounter, 0).asInstanceOf[Int]
        val key = Comment(counter)
        val newState = withMap.map - CommentCounter + (CommentCounter -> (counter + 1))
        val value = withMap.map.get(key) match {
          case Some(comment) => comment.asInstanceOf[Seq[String]]
          case _ => Seq.empty
        }
        Some(WithMapG(value, newState))
    }, showMap = true)
  }
//  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
//    val commentsGrammar = grammars.create(CommentGrammar, getCommentsGrammar)
//
//    for(path <- grammars.root.selfAndDescendants.
//      filter(path => path.get.isInstanceOf[Layout]))
//    {
//      addCommentPrefixToGrammar(commentsGrammar, path.asInstanceOf[GrammarReference])
//    }
//    val node = grammars.root.find(p => p.get.isInstanceOf[NodeGrammar]).get.get.asInstanceOf[NodeGrammar]
//    //node.inner = commentsGrammar ~> node.inner
//  }
//
//  def addCommentPrefixToGrammar(commentsGrammar: BiGrammar, layoutReference: GrammarReference): Unit = {
//    layoutReference.get match {
//      case sequence: SequenceLike => sequence.second =
//        if (sequence.horizontal) commentsGrammar ~> sequence.second
//        else commentsGrammar %> sequence.second
//      case many: ManyVertical => layoutReference.set(many.inner.manySeparatedVertical(commentsGrammar))
//      case many: ManyHorizontal => layoutReference.set(many.inner.manySeparated(commentsGrammar))
//    }
//  }

//  def getCommentsGrammar: BiGrammar = {
//    val commentGrammar = getCommentGrammar
//    val comments = commentGrammar.manyVertical ^^
//      (comments => CommentCollection(comments.asInstanceOf[Seq[String]]), {
//        case c: CommentCollection => Some(c.comments)
//        case _ => None
//      })
//    comments .as(CommentKey) | value(Unit)
//  }

  def getCommentGrammar: BiGrammar = {
    val regex: Regex = new Regex( """/\*.*\*/""")
    RegexG(regex) ~ space
  }

  override def description: String = "Adds Java-style comments to the language"
}
