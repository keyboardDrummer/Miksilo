package core.deltas

import core.language.node.Key
import core.language.Language

import scala.collection.mutable.ArrayBuffer

trait Delta extends Contract with Key {
  def description: String

  def suffix = "Delta"

  def inject(language: Language): Unit = {}
}

object Delta {
  def buildLanguage(topToBottom: Seq[Delta]): Language = {
    val explicitDeltas = topToBottom.reverse
    val allDeltas = validateDependencies(explicitDeltas)
    val result = new Language()
    for(delta <- allDeltas)
    {
      delta.inject(result)
    }
    result
  }

  //Bad order error
  //All missing dependencies.
  def validateDependencies(explicitDeltas: Seq[Delta]): Seq[Delta] = {
    var available = Set.empty[Contract]
    var allDeltas = ArrayBuffer.empty[Delta]

    def addDelta(delta: Delta, detectCycleSet: Set[Delta]): Unit = {
      delta.dependencies.foreach(dependency =>
        if (!available.contains(dependency)) {
          dependency match {
            case deltaDependency: Delta =>
              if (detectCycleSet.contains(deltaDependency))
                throw DeltaDependencyViolation(dependency, delta)
              else
                addDelta(deltaDependency, detectCycleSet + delta)
            case _ =>
              throw DeltaDependencyViolation(dependency, delta)
          }
        }
      )
      available += delta
      allDeltas += delta
    }

    for (delta <- explicitDeltas) {
      addDelta(delta, Set.empty)
    }
    allDeltas
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