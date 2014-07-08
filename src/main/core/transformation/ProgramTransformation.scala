package core.transformation

import core.grammar.{Grammar, GrammarWriter}

import scala.collection.mutable

trait GrammarTransformation extends ProgramTransformation with GrammarWriter
{
  def transform(program: MetaObject, state: TransformationState) = {}
  def dependencies: Set[ProgramTransformation] = Set.empty
  def transformGrammar(grammar: Grammar) : Grammar
  def transformReserved(reserved: mutable.HashSet[String]) = {}
  def transformDelimiters(delimiters: mutable.HashSet[String]) = {}
}

trait ProgramTransformation {
  def transform(program: MetaObject, state: TransformationState)
  def dependencies: Set[ProgramTransformation]
}
