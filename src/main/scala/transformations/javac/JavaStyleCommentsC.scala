package transformations.javac

import core.bigrammar._
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.NodeField
import core.particles.{DeltaWithGrammar, Language}

import scala.util.matching.Regex

object JavaStyleCommentsC extends DeltaWithGrammar {

  case class CommentCollection(comments: Seq[String])

  object CommentKey extends NodeField
  object CommentGrammar
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val commentsGrammar = grammars.create(CommentGrammar, getCommentsGrammar)

    for(path <- new RootGrammar(grammars.find(ProgramGrammar)).selfAndDescendants.
      filter(path => path.get.isInstanceOf[NodeGrammar]))
    {
      addCommentPrefixToGrammar(commentsGrammar, path.children.head)
    }
  }

  def addCommentPrefixToGrammar(commentsGrammar: BiGrammar, grammarReference: GrammarReference): Unit = {
    val verticalNotHorizontal: Boolean = getCommentVerticalOrHorizontal(grammarReference)
    val newGrammar = if (verticalNotHorizontal) commentsGrammar %> grammarReference.get
                           else commentsGrammar ~> grammarReference.get
    grammarReference.set(newGrammar)
  }

  def getCommentVerticalOrHorizontal(nodeMapPath: GrammarReference): Boolean = {
    val growers = nodeMapPath.ancestors.map(path => path.get).
      filter(grammar => grammar.isInstanceOf[TopBottom] || grammar.isInstanceOf[Sequence] || grammar.isInstanceOf[ManyVertical] || grammar.isInstanceOf[ManyHorizontal])

    val verticalNotHorizontal = growers.nonEmpty && {
      val firstGrower = growers.head
      firstGrower.isInstanceOf[TopBottom] || firstGrower.isInstanceOf[ManyVertical]
    }
    verticalNotHorizontal
  }

  def getCommentsGrammar: BiGrammar = {
    val commentGrammar = getCommentGrammar
    val comments = commentGrammar.manyVertical ^^
      (comments => CommentCollection(comments.asInstanceOf[Seq[String]]), {
        case c: CommentCollection => Some(c.comments)
        case _ => None
      })
    comments.as(CommentKey)
  }

  def getCommentGrammar: BiGrammar = {
    val regex: Regex = new Regex( """/\*.*\*/""")
    RegexG(regex) ~ space
  }

  override def description: String = "Adds Java-style comments to the language"
}
