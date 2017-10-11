package transformations.javac

import core.bigrammar._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.NodeField
import core.particles.{DeltaWithGrammar, Language}

import scala.util.matching.Regex

object JavaStyleCommentsC extends DeltaWithGrammar {

  object CommentKey extends NodeField
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

  def getCommentsGrammar: BiGrammar = {
    val commentGrammar = getCommentGrammar
    val comments = commentGrammar.manyVertical
    new MapGrammar(comments, { case withMap: WithMap =>
      val existingCommentsOption = withMap.state.get(CommentKey)
      val newComments: Seq[String] = existingCommentsOption.fold(withMap.value.asInstanceOf[Seq[String]])(
          { case existingComments: Seq[String] => Seq(withMap.value.asInstanceOf[String]) ++ existingComments })
      val newState = if (newComments.isEmpty) withMap.state else withMap.state + (CommentKey -> newComments)
      WithMap(Unit, newState)
    }, {
      case withMap: WithMap2 => withMap.state.get(CommentKey) match {
        case Some(comment) =>
          Some(WithMap2(comment.asInstanceOf[Seq[String]], withMap.state.remove(CommentKey)))
        case _ => Some(WithMap2(Seq.empty, withMap.state))
      }
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
