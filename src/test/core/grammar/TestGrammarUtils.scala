package core.grammar

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import core.transformation._
import scala.collection.mutable

class TestGrammarUtils {

  val manager: TransformationManager = new TransformationManager()

  def buildParser(transformations: Seq[GrammarTransformation], selector: Grammar => Grammar): String => manager.ParseResult[Any] = {

    object SelectorTransformation extends GrammarTransformation {
      override def transformGrammar(grammar: Grammar): Grammar = selector(grammar)
      override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = {}
      override def transformReserved(reserved: mutable.HashSet[String]): Unit = {}
      override def transform(program: MetaObject, state: TransformationState): Unit = {}
      override def dependencies: Set[ProgramTransformation] = Set.empty
    }

    manager.buildParser(transformations ++ Seq(SelectorTransformation))
  }
}
