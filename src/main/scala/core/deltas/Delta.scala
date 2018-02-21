package core.deltas

import core.language.node.Key
import core.language.Language

trait Delta extends Contract with Key {
  def description: String

  def suffix = "Delta"

  def inject(language: Language): Unit = {  }
}

object Delta {
  def buildLanguage(deltas: Seq[Delta]): Language = {
    validateDependencies(deltas)
    val result = new Language()
    for(delta <- deltas.reverse)
    {
      delta.inject(result)
    }
    result
  }

  //Bad order error
  //All missing dependencies.
  def validateDependencies(deltas: Seq[Delta]): Unit = {
    var available = Set.empty[Contract]
    for (delta <- deltas.reverse) {
      delta.dependencies.foreach(dependency =>
        if (!available.contains(dependency))
          throw DeltaDependencyViolation(dependency, delta)
      )
      available += delta
    }
  }

  def replace(deltas: Seq[Delta], marker: Delta, splice: Seq[Delta]): Seq[Delta] = {
    val pivot = deltas.indexWhere(particle => marker == particle)
    val (before,after) = deltas.splitAt(pivot)
    before ++ splice ++ after.drop(1)
  }

  def spliceAndFilterTop(top: Seq[Delta], bottom: Seq[Delta], splice: Seq[Delta] = Seq.empty): Seq[Delta] = {
    val implicitsSet = bottom.toSet
    top.filter(t => !implicitsSet.contains(t)) ++ splice ++ bottom
  }

  def spliceAndFilterBottom(top: Seq[Delta], bottom: Seq[Delta], splice: Seq[Delta] = Seq.empty): Seq[Delta] = {
    val implicitsSet = top.toSet
    top ++ splice ++ bottom.filter(t => !implicitsSet.contains(t))
  }
}