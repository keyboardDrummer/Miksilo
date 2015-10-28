package transformations.javac

import core.bigrammar.BiGrammar
import core.grammar.RegexG
import core.particles.ParticleWithGrammar
import core.particles.grammars.{ProgramGrammar, GrammarCatalogue}
import core.particles.node.{Key, Node}
import transformations.javac.methods.MethodC
import transformations.javac.methods.MethodC.VisibilityGrammar

import scala.util.matching.Regex

object JavaCommentsC extends ParticleWithGrammar {

  case class CommentCollection(comments: Seq[String])

  object CommentKey extends Key
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val methodGrammar = grammars.find(MethodC.MethodGrammar)

    val comment: BiGrammar = commentGrammar
    val comments: BiGrammar = comment.many ^^
      (comments => CommentCollection(comments.asInstanceOf[Seq[String]]),
        commentCollection => Some(commentCollection.asInstanceOf[CommentCollection].comments))
    val methodNodeMap = methodGrammar.inner.asInstanceOf[NodeMap]
    val newInner = comments % methodNodeMap.inner ^^ (addComment, removeComment)
    val newNodeMap = new NodeMap(newInner, methodNodeMap.key, Seq(CommentKey) ++ methodNodeMap.fields:_*)
    methodGrammar.inner = newNodeMap
  }

  def commentGrammar: BiGrammar = {
    val regex: Regex = new Regex( """/\*.*\*/""")
    new RegexG(regex)
  }

  def addComment(from: Any): Any = {
    val values = tildeValuesToSeq(from)
    val commentCollections = values.collect({case x : CommentCollection => x})
    val combinedComment = CommentCollection(commentCollections.flatMap(commentCollection => commentCollection.comments))
    val nonComments = values.filter(x => !x.isInstanceOf[CommentCollection])
    val newValues = Seq(combinedComment) ++ nonComments
    newValues.reduce((a,b) => core.grammar.~(a,b))
  }

  def removeComment(from: Any): Option[Any] = {
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
