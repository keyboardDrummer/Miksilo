package core.transformation

import core.grammar.{GrammarWriter, Grammar}
import scala.util.parsing.combinator.Parsers
import java.util
import scala.collection.mutable

trait GrammarTransformation extends ProgramTransformation with GrammarWriter
{
  def transformGrammar(grammar: Grammar) : Grammar
  def transformReserved(reserved: mutable.HashSet[String])
  def transformDelimiters(delimiters: mutable.HashSet[String])
}

trait ProgramTransformation {
  def transform(program: MetaObject, state: TransformationState)
  def dependencies: Set[ProgramTransformation]
}
