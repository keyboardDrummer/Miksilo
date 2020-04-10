package miksilo.editorParser.languages.yaml

import miksilo.editorParser.document.Empty
import miksilo.editorParser.parsers.SourceElement
import miksilo.editorParser.parsers.editorParsers.OffsetPointerRange
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

trait YamlValue extends SourceElement {
  def toDocument: ResponsiveDocument

  override def toString: String = toDocument.renderString()
}

case class YamlObject(rangeOption: Option[OffsetPointerRange], members: Array[(YamlValue, YamlValue)]) extends YamlValue {
  override def toDocument: ResponsiveDocument = {
    members.
      map(member => member._1.toDocument ~ ":" ~~ member._2.toDocument).
      fold[ResponsiveDocument](Empty)((t,b) => t % b)
  }
}

case class YamlArray(rangeOption: Option[OffsetPointerRange],elements: Array[YamlValue]) extends YamlValue {
  override def toDocument: ResponsiveDocument = {
    elements.
      map(member => ResponsiveDocument.text("- ") ~~ member.toDocument).
      fold[ResponsiveDocument](Empty)((t: ResponsiveDocument, b: ResponsiveDocument) => t % b)
  }
}

case class NumberLiteral(rangeOption: Option[OffsetPointerRange], value: String) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

case class StringLiteral(rangeOption: Option[OffsetPointerRange],value: String) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

case class ValueHole(range: OffsetPointerRange) extends YamlValue {
  override def toDocument: ResponsiveDocument = "hole"

  override def rangeOption = Some(range)
}

case class TaggedNode(rangeOption: Option[OffsetPointerRange],tag: String, node: YamlValue) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text("!") ~ tag ~~ node.toDocument
}