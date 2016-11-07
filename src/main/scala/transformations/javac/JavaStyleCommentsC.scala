package transformations.javac

import core.bigrammar._
import core.grammar.RegexG
import core.particles.{MapInsideNode, ParticleWithGrammar}
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Key
import util.DataFlowAnalysis

import scala.util.matching.Regex

object JavaStyleCommentsC extends ParticleWithGrammar {

  case class CommentCollection(comments: Seq[String])

  object CommentKey extends Key
  object CommentGrammar
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

    val commentsGrammar = grammars.create(CommentGrammar, getCommentsGrammar.as(CommentKey))

    for(path <- new RootGrammar(grammars.find(ProgramGrammar)).descentsIncludingSelf)
    {
      path match {
        case selection: GrammarSelection =>
          val current = selection.get
          current match {
//            case _:Keyword =>
//              selection.set(new Sequence(commentsGrammar, current))
//            case _:Delimiter =>
//              selection.set(new Sequence(commentsGrammar, current))
            case nodeMap:NodeMap =>
              if (nodeMap.key != MapInsideNode)
              {
                addCommentToNodeMap(commentsGrammar, selection)
              }
//            case _:MapGrammar =>
//              selection.set(new Sequence(commentsGrammar, current))
            case _ =>
          }
        case _ =>
      }
    }
  }

  def addCommentToNodeMap(commentsGrammar: BiGrammar, nodeMapPath: GrammarSelection): Unit = {
    val nodeMapToTransform = nodeMapPath.get.asInstanceOf[NodeMap]
    val verticalNotHorizontal: Boolean = getCommentVerticalOrHorizontal(nodeMapPath)

    val innerWithComment = if (verticalNotHorizontal) commentsGrammar %> nodeMapToTransform.inner
                           else commentsGrammar ~> nodeMapToTransform.inner
    nodeMapToTransform.inner = innerWithComment
  }

  def getCommentVerticalOrHorizontal(nodeMapPath: GrammarSelection): Boolean = {
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
    commentGrammar.manyVertical ^^
      (comments => CommentCollection(comments.asInstanceOf[Seq[String]]),
        commentCollection => Some(commentCollection.asInstanceOf[CommentCollection].comments))
  }

  def getCommentGrammar: BiGrammar = {
    val regex: Regex = new Regex( """/\*.*\*/""")
    RegexG(regex) ~ space
  }

  override def description: String = "Adds Java-style comments to the language"
}
