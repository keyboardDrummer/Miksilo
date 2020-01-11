package core.deltas

import core.language.Language
import core.language.node.Key

trait Delta extends Contract with Key {
  def description: String

  def suffix = "Delta"

  def inject(language: Language): Unit = {}
}

object Delta {

  def insertUnder(deltas: Seq[Delta], marker: Delta, insert: Seq[Delta]): Seq[Delta] = {
    val pivot = deltas.indexWhere(particle => marker == particle)
    val (before,after) = deltas.splitAt(pivot + 1)
    before ++ insert ++ after
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

