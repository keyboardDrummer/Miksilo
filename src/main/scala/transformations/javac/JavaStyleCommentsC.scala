package transformations.javac

import core.bigrammar.BiGrammarToGrammar.{Result, StateM}
import core.bigrammar._
import core.bigrammar.printer.TryState.{NodePrinter, State}
import core.grammar.Grammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{GrammarKey, Key, NodeField}
import core.particles.{DeltaWithGrammar, Language, NodeGrammar}
import core.responsiveDocument.ResponsiveDocument

import scala.util.Try
import scala.util.matching.Regex

object JavaStyleCommentsC extends DeltaWithGrammar {


  case class NodeWrapper(var node: BiGrammar) extends SuperCustomGrammar {

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

    override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = NodeWrapper(newChildren.head)
  }

  object CommentGrammar extends GrammarKey {
    override lazy val toString: String = "Comments"
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val commentsGrammar = grammars.create(CommentGrammar, CommentsGrammar)

    var visited = Set.empty[BiGrammar]
    for(path <- grammars.root.selfAndDescendants)
    {
      if (!visited.contains(path.get)) {
        visited += path.get
        if (path.get.isInstanceOf[Layout]) {
          addCommentPrefixToGrammar(commentsGrammar, path.asInstanceOf[GrammarReference])
        }
      }
    }

    var visited2 = Set.empty[BiGrammar]
    for(path <- grammars.root.selfAndDescendants)
    {
      if (!visited2.contains(path.get)) {
        visited2 += path.get
        if (path.get.isInstanceOf[NodeGrammar]) {
          val node = path.get.asInstanceOf[NodeGrammar]
          path.asInstanceOf[GrammarReference].set(NodeWrapper(node))
        }
      }
    }

    val node = grammars.root.find(p => p.get.isInstanceOf[NodeGrammar]).get.get.asInstanceOf[NodeGrammar]
    node.inner = commentsGrammar ~> node.inner
    System.out.append("")
  }

  def addCommentPrefixToGrammar(commentsGrammar: BiGrammar, layoutReference: GrammarReference): Unit = {
    layoutReference.get match {
      case sequence: SequenceLike =>
        if (!isOk(sequence.first, commentsGrammar) || !isOk(sequence.second, commentsGrammar))
          return

        injectComments(commentsGrammar, layoutReference.children(1), sequence.horizontal)
      case many: ManyVertical => injectComments(commentsGrammar, layoutReference.children.head, horizontal = false)
      case many: ManyHorizontal => injectComments(commentsGrammar, layoutReference.children.head, horizontal = true)
      case _ =>
    }
  }

  def injectComments(commentsGrammar: BiGrammar, grammar: GrammarReference, horizontal: Boolean): Boolean = {
    if (grammar.ancestors.size + 1 != grammar.ancestorGrammars.size) {
      return true
    }

    grammar.get match {
      case _: BiFailure => false
      case _: Print => false
      case _: ValueGrammar => false
      case _: Labelled => injectComments(commentsGrammar, grammar.children.head, horizontal)
      case superCustom: SuperCustomGrammar if superCustom.children.size == 1 =>
        injectComments(commentsGrammar, grammar.children.head, horizontal)
      case node: NodeGrammar =>
        node.inner =
          if (horizontal) commentsGrammar ~> node.inner
          else commentsGrammar %> node.inner
        true
      case _: MapGrammar => injectComments(commentsGrammar, grammar.children.head, horizontal)
      case _: As => injectComments(commentsGrammar, grammar.children.head, horizontal)
      case _: SequenceLike =>
        if (!injectComments(commentsGrammar, grammar.children.head, horizontal))
          injectComments(commentsGrammar, grammar.children(1), horizontal)
        else
          true
      case _: Choice =>
        injectComments(commentsGrammar, grammar.children.head, horizontal) |
          injectComments(commentsGrammar, grammar.children(1), horizontal)
      case _ => grammar.set(
        if (horizontal) commentsGrammar ~> grammar.get
        else commentsGrammar %> grammar.get)
        true
    }
  }

  def isOk(grammar: BiGrammar, commentGrammar: BiGrammar): Boolean = {
    if (grammar == commentGrammar)
      return false

    grammar match {
      case _:ValueGrammar => false
      case _:Print => false
      case _: BiFailure => false
      case _ => true
    }
  }

  object CommentCounter extends Key
  case class Comment(index: Int) extends NodeField

  object CommentsGrammar extends SuperCustomGrammar with BiGrammarWithoutChildren {

    val commentGrammar = getCommentGrammar
    var comments = commentGrammar.manyVertical

    override def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar = {
      recursive(comments) ^^ {
        case result: Result => StateM((state: BiGrammarToGrammar.State) => {
          val counter: Int = state.getOrElse(CommentCounter, 0).asInstanceOf[Int]
          val key = Comment(counter)
          val newState = state + (CommentCounter -> (counter + 1))
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
          case Some(comment) =>
            comment.asInstanceOf[Seq[String]]
          case _ => Seq.empty
        }
        commentsPrinter.write(WithMapG(value, from.map), newState)
      }
    }
  }

  def getCommentGrammar: BiGrammar = {
    val regex: Regex = new Regex("""(?s)/\*.*?\*/""")
    RegexG(regex) ~ space
  }

  override def description: String = "Adds Java-style comments to the language"
}
