package transformations.javac

object TriviaInsideNode {

  //    var visited = Set.empty[BiGrammar]
  //    for(path <- grammars.root.selfAndDescendants)
  //    {
  //      if (!visited.contains(path.get)) {
  //        visited += path.get
  //        if (path.get.isInstanceOf[Layout]) {
  //          addCommentPrefixToGrammar(commentsGrammar, path.asInstanceOf[GrammarReference])
  //        }
  //      }
  //    }

  //  def addCommentPrefixToGrammar(commentsGrammar: BiGrammar, layoutReference: GrammarReference): Unit = {
  //    layoutReference.get match {
  //      case sequence: SequenceLike =>
  //        if (!isOk(sequence.first, commentsGrammar) || !isOk(sequence.second, commentsGrammar))
  //          return
  //
  //        injectComments(commentsGrammar, layoutReference.children(1), sequence.horizontal)
  //      case many: ManyVertical => injectComments(commentsGrammar, layoutReference.children.head, horizontal = false)
  //      case many: ManyHorizontal => injectComments(commentsGrammar, layoutReference.children.head, horizontal = true)
  //      case _ =>
  //    }
  //  }
  //
  //  def injectComments(commentsGrammar: BiGrammar, grammar: GrammarReference, horizontal: Boolean): Boolean = {
  //    if (grammar.ancestors.size + 1 != grammar.ancestorGrammars.size) {
  //      return true
  //    }
  //
  //    grammar.get match {
  //      case CommentsGrammar => true
  //      case _: BiFailure => false
  //      case _: Print => false
  //      case _: ValueGrammar => false
  //      case _: Labelled => injectComments(commentsGrammar, grammar.children.head, horizontal)
  //      case superCustom: SuperCustomGrammar if superCustom.children.size == 1 =>
  //        injectComments(commentsGrammar, grammar.children.head, horizontal)
  //      case node: NodeGrammar => node.inner match {
  //        case ignore: IgnoreLeft if ignore.inner.isInstanceOf[SequenceLike] &&
  //          ignore.inner.asInstanceOf[SequenceLike].first == CommentsGrammar =>
  //          true
  //        case _ =>
  //          node.inner =
  //            if (horizontal) commentsGrammar ~> node.inner
  //            else commentsGrammar %> node.inner
  //          true
  //      }
  //      case _: MapGrammar => injectComments(commentsGrammar, grammar.children.head, horizontal)
  //      case _: As => injectComments(commentsGrammar, grammar.children.head, horizontal)
  //      case _: SequenceLike =>
  //        if (!injectComments(commentsGrammar, grammar.children.head, horizontal))
  //          injectComments(commentsGrammar, grammar.children(1), horizontal)
  //        else
  //          true
  //      case _: Choice =>
  //        injectComments(commentsGrammar, grammar.children.head, horizontal) |
  //          injectComments(commentsGrammar, grammar.children(1), horizontal)
  //      case _ => grammar.set(
  //        if (horizontal) commentsGrammar ~> grammar.get
  //        else commentsGrammar %> grammar.get)
  //        true
  //    }
  //  }
  //
  //  def isOk(grammar: BiGrammar, commentGrammar: BiGrammar): Boolean = {
  //    if (grammar == commentGrammar)
  //      return false
  //
  //    grammar match {
  //      case _:ValueGrammar => false
  //      case _:Print => false
  //      case _: BiFailure => false
  //      case _ => true
  //    }
  //  }
}
