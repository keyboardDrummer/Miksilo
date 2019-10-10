package core.bigrammar

import core.bigrammar.grammars._
import core.document.WhiteSpace
import core.language.node.GrammarKey
import util.{GraphBasics, Utility}

import scala.reflect.ClassTag

/*
A grammar that maps to both a parser and a printer
 */

object BiGrammar {

  type State = Map[Any, Any]

  def someMap[Value](grammar: BiGrammar[(Value, Seq[Value])]): BiGrammar[Seq[Value]] = {
    grammar.mapSome(
      t => Seq(t._1) ++ t._2,
      seq => if (seq.nonEmpty) Some((seq.head, seq.tail)) else None)
  }

  def flattenOptionSeq[T](grammar: BiGrammar[Option[Seq[T]]]): BiGrammar[Seq[T]] = grammar.map(
    option => option.fold[Seq[T]](Seq.empty)(v => v),
    seq => if (seq.isEmpty) None else Some(seq))

  def optionToSeq[T](grammar: BiGrammar[Option[T]]): BiGrammar[Seq[T]] = grammar.map(
    option => option.fold[Seq[T]](Seq.empty)(v => Seq(v)),
    seq => seq.headOption)

  def seqToSet[T](grammar: BiGrammar[Seq[T]]): BiGrammar[Set[T]] = grammar.map(
    seq => seq.toSet,
    set => set.toSeq)
}

trait BiGrammar[Value] {
  import BiGrammarWriter._

  override def toString: String = PrintBiGrammar.toDocument(this).renderString(trim = false)

  lazy val height = 1

  def |(other: BiGrammar[Value]) = new BiChoice[Value](this, other, !other.containsParser())
  def option: BiGrammar[Option[Value]] = new BiChoice(this.mapSome(x => Some(x), x => x), value(None), true)

  def indent(width: Int = 2): BiGrammar[Value] =
    leftRight(WhiteSpace(width, 0), this, BiSequence.ignoreLeft)

  def children: Seq[BiGrammar[_]]
  def withChildren(newChildren: Seq[BiGrammar[_]]): BiGrammar[Value]
  def deepMap(function: BiGrammar[_] => BiGrammar[_]): BiGrammar[_] = new BiGrammarObserver[BiGrammar[_]] {

    override def getReference(name: GrammarKey): BiGrammar[_] = new Labelled[Any](name)

    override def setReference(result: BiGrammar[_], reference: BiGrammar[_]): Unit = {
      reference.asInstanceOf[Labelled[Any]].inner = result.asInstanceOf[Labelled[Any]].inner
    }

    override def handleGrammar(self: BiGrammar[_], children: Seq[BiGrammar[_]], recursive: BiGrammar[_] => BiGrammar[_]): BiGrammar[_] =
      self.withChildren(children)
  }.observe(this)


  def map[NewValue: ClassTag](afterParsing: Value => NewValue, beforePrinting: NewValue => Value): BiGrammar[NewValue] =
    this.mapSome(afterParsing, (u: NewValue) => Some(beforePrinting(u)))

  def mapSome[NewValue: ClassTag](afterParsing: Value => NewValue, beforePrinting: NewValue => Option[Value]): BiGrammar[NewValue] =
    new MapGrammar[Value, NewValue](this,
      value => Right(afterParsing(value)),
      value => Utility.cast[NewValue](value).flatMap(value => beforePrinting(value)))

  def deepClone: BiGrammar[Value] = deepMap(x => x).asInstanceOf[BiGrammar[Value]]
  def containsParser(): Boolean = {
    var map: Map[BiGrammar[_], Boolean] = Map.empty
    lazy val recursive: BiGrammar[_] => Boolean = grammar => {
      map.get(grammar) match {
        case Some(result) => result
        case _ =>
          map += grammar -> false
          val result = grammar.containsParser(recursive)
          map += grammar -> result
          result
      }
    }
    this.containsParser(recursive)
  }

  def isLeftRecursive: Boolean = {
    var isRecursive = false
    var seen = Set.empty[BiGrammar[_]]
    lazy val recursive: BiGrammar[_] => Seq[BiGrammar[_]] = grammar => {
      if (grammar == this || isRecursive) {
        isRecursive = true
        Seq.empty
      }
      else {
        if (!seen.contains(grammar)) {
          seen += grammar
          grammar.getLeftChildren(recursive)
        } else
          Seq.empty
      }
    }
    this.getLeftChildren(recursive)
    isRecursive
  }

  def getLeftChildren: Seq[BiGrammar[_]] = {
    var map: Map[BiGrammar[_], Seq[BiGrammar[_]]] = Map.empty
    lazy val recursive: BiGrammar[_] => Seq[BiGrammar[_]] = grammar => {
      map.get(grammar) match {
        case Some(result) => result
        case _ =>
          map += grammar -> Seq.empty
          val result = Seq(grammar) ++ grammar.getLeftChildren(recursive)
          map += grammar -> result
          result
      }
    }
    Seq(this) ++ this.getLeftChildren(recursive)
  }

  protected def getLeftChildren(recursive: BiGrammar[_] => Seq[BiGrammar[_]]): Seq[BiGrammar[_]] =
    children.flatMap(c => recursive(c))

  def containsParser(recursive: BiGrammar[_] => Boolean): Boolean

  def selfAndDescendants: Seq[BiGrammar[_]] = GraphBasics.traverseBreadth[BiGrammar[_]](Seq(this), grammar => grammar.children)
}
