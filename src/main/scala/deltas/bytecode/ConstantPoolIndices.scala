package deltas.bytecode

import core.bigrammar._
import core.bigrammar.grammars.ManyVertical
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass, NodeField}
import core.deltas.{Contract, DeltaWithGrammar, Language}
import deltas.bytecode.ByteCodeSkeleton.{ConstantPoolGrammar, ConstantPoolItemContentGrammar}

object ConstantPoolIndices extends DeltaWithGrammar {

  override def description: String = "Add indices to the constant pool entries"

  override def dependencies: Set[Contract] = Set[Contract](ByteCodeSkeleton)

  private object WithIndexClass extends NodeClass

  private object Index extends NodeField

  private object Content extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val previousConstantPoolItem = find(ByteCodeSkeleton.ConstantPoolItemContentGrammar)
    val constantPoolItem = (("#" ~> number.as(Index) ~~< "=") ~~ previousConstantPoolItem.inner.as(Content)).
      asNode(WithIndexClass)
    previousConstantPoolItem.inner = constantPoolItem

    val constantPoolGrammar = find(ConstantPoolGrammar)
    val entries: GrammarReference = new RootGrammar(constantPoolGrammar).findLabelled(ConstantPoolItemContentGrammar).ancestors.filter(p => p.value.isInstanceOf[ManyVertical]).head.asInstanceOf[GrammarReference]
    entries.set(addIndicesToList(entries.value))
  }

  def addIndicesToList(listGrammar: BiGrammar): BiGrammar = {
    val removeIndexForParsing: (Any) => Seq[Any] = items => items.asInstanceOf[Seq[Node]].map(i => i(Content))
    val addIndexForPrinting: (Any) => Some[Seq[Node]] = items => Some(items.asInstanceOf[Seq[Any]].zipWithIndex.map((p: (Any, Int)) =>
      WithIndexClass.create(Index -> (p._2 + 1), Content -> p._1)))
    listGrammar ^^ ( removeIndexForParsing, addIndexForPrinting )
  }
}
