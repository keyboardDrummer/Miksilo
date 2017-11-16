package core.bigrammar

import core.bigrammar.grammars.{Choice, Labelled, MapGrammar, Sequence}
import core.document.WhiteSpace
import core.deltas.node.GrammarKey

/*
A grammar that maps to both a parser and a printer
 */
trait BiGrammar extends BiGrammarWriter {

  override def toString = PrintBiGrammar.toDocument(this).renderString(trim = false)

  lazy val height = 1

  def |(other: BiGrammar) = new Choice(this, other)
  def option: BiGrammar = this ^^ (x => Some(x), x => x.asInstanceOf[Option[Any]]) | value(None)

  def indent(width: Int = 2) = new Sequence(WhiteSpace(width, 0), this).ignoreLeft

  def optionToSeq: BiGrammar = new MapGrammar(this,
    option => option.asInstanceOf[Option[Any]].fold(Seq.empty[Any])(x => Seq(x)), {
      case seq: Seq[Any] => Some(if (seq.isEmpty) None else Some(seq))
      case _ => None
    })
  def seqToSet: BiGrammar = new MapGrammar(this,
    seq => seq.asInstanceOf[Seq[Any]].toSet,
    set => Some(set.asInstanceOf[Set[Any]].toSeq))

  def ^^(afterParsing: Any => Any, beforePrinting: Any => Option[Any]): BiGrammar =
    new MapGrammar(this, afterParsing, beforePrinting)

  def children: Seq[BiGrammar]
  def withChildren(newChildren: Seq[BiGrammar]): BiGrammar
  def map(function: BiGrammar => BiGrammar): BiGrammar = new BiGrammarObserver[BiGrammar] {
    override def getReference(name: GrammarKey): BiGrammar = new Labelled(name)

    override def setReference(result: BiGrammar, reference: BiGrammar): Unit = {
      reference.asInstanceOf[Labelled].inner = result.asInstanceOf[Labelled].inner
    }

    override def handleGrammar(self: BiGrammar, children: Seq[BiGrammar], recursive: (BiGrammar) => BiGrammar): BiGrammar = self.withChildren(children)
  }.observe(this)

  def deepClone = map(x => x)
  def containsParser(): Boolean = {
    var map: Map[BiGrammar, Boolean] = Map.empty
    lazy val recursive: BiGrammar => Boolean = grammar => {
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

  protected def containsParser(recursive: BiGrammar => Boolean): Boolean
}
