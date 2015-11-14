package transformations.javac

import core.bigrammar._
import core.grammar.RegexG
import core.particles.ParticleWithGrammar
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Key
import util.DataFlowAnalysis

import scala.util.matching.Regex

object JavaCommentsC extends ParticleWithGrammar {

  class GrammarAnalysis extends DataFlowAnalysis[GrammarPath,Boolean]
  {

    val biGrammarClass = classOf[BiGrammar]
    override def getOutgoingNodes(path: GrammarPath): Set[GrammarPath] = {
      path.children.toSet
    }

    override def updateState(state: Boolean, path: GrammarPath): Boolean = {
      path.get match {
        case _: NodeMap => true
        case _: MapGrammar => false
        case _ => state
      }
    }

    override def combineState(first: Boolean, second: Boolean): Option[Boolean] = {
      if (first == second)
        return None
      
      Some(first && second)
    }
  }
  
  case class CommentCollection(comments: Seq[String])

  val commentsGrammar = getCommentsGrammar
  object CommentKey extends Key
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {

    val analysis = new GrammarAnalysis()
    analysis.run(new Root(grammars.find(ProgramGrammar)), false)
    
    for(pathWithState <- analysis.states)
    {
      val path = pathWithState._1
      path match {
        case selection: GrammarSelection =>
          val current = selection.get
          current match {
//            case _:Keyword =>
//              selection.set(new Sequence(commentsGrammar, current))
//            case _:Delimiter =>
//              selection.set(new Sequence(commentsGrammar, current))
            case _:NodeMap =>
              addCommentToNodeMap(selection.parent.asInstanceOf[Labelled])
//            case _:MapGrammar =>
//              selection.set(new Sequence(commentsGrammar, current))
            case _ =>
          }
        case _ =>
      }
    }
  }

  def addCommentToNodeMap(labelledWithNodeMap: Labelled): Unit = {
    val nodeMap = labelledWithNodeMap.inner.asInstanceOf[NodeMap]
    val newInner = new TopBottom(commentsGrammar, nodeMap.inner) ^^ (combineCommentsAndPlaceLeft, replaceLeftValueNotFoundWithEmptyComment)
    val newNodeMap = new NodeMap(newInner, nodeMap.key, Seq(CommentKey) ++ nodeMap.fields: _*)
    labelledWithNodeMap.inner = newNodeMap
  }

  def getCommentsGrammar: BiGrammar = {
    val commentGrammar = getCommentGrammar
    commentGrammar.many ^^
      (comments => CommentCollection(comments.asInstanceOf[Seq[String]]),
        commentCollection => Some(commentCollection.asInstanceOf[CommentCollection].comments))
  }

  def getCommentGrammar: BiGrammar = {
    val regex: Regex = new Regex( """/\*.*\*/""")
    new RegexG(regex)
  }

  def combineCommentsAndPlaceLeft(from: Any): Any = {
    val values = tildeValuesToSeq(from)
    val commentCollections = values.collect({case x : CommentCollection => x})
    val combinedComment = CommentCollection(commentCollections.flatMap(commentCollection => commentCollection.comments))
    val nonComments = values.filter(x => !x.isInstanceOf[CommentCollection])
    val newValues = Seq(combinedComment) ++ nonComments
    newValues.reduce((a,b) => core.grammar.~(a,b))
  }

  def replaceLeftValueNotFoundWithEmptyComment(from: Any): Option[Any] = {
    val values = tildeValuesToSeq(from)
    val withoutComment = values.drop(1)
    val comment = values.head match {
      case collection: CommentCollection => collection
      case ValueNotFound => CommentCollection(Seq.empty)
    }
    Some(core.grammar.~(comment, withoutComment.reduce((a,b) => core.grammar.~(a,b))))
  }

  override def description: String = "Adds Java-style comments to the language"
}
