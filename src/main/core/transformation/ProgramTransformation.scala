package core.transformation

import core.grammar.{Grammar, GrammarWriter, Labelled}

import scala.collection.mutable

class GrammarCatalogue {
  var grammars: Map[Any, Labelled] = Map.empty

  def find(key: Any): Labelled = grammars(key)

  def create(key: AnyRef, inner: Grammar = null): Labelled = {
    val result = new Labelled(key, inner)
    grammars += key -> result
    result
  }
}

trait GrammarTransformation extends ProgramTransformation with GrammarWriter {

  def transform(program: MetaObject, state: TransformationState) = {}

  def dependencies: Set[Contract] = Set.empty

  def transformGrammars(grammars: GrammarCatalogue)

  def transformReserved(reserved: mutable.HashSet[String]) = {}

  def transformDelimiters(delimiters: mutable.HashSet[String]) = {}
}


trait ProgramTransformation extends Contract {
  def transform(program: MetaObject, state: TransformationState)

}
