package application.compilerBuilder

import javax.swing.RowFilter.Entry
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.text.Document
import javax.swing.{ListModel, RowFilter}

import core.deltas.Delta
import org.jdesktop.swingx.JXList


class ContactRowFilter(val compare: String) extends RowFilter[ListModel[_], java.lang.Integer] {
  override def include(entry: Entry[_ <: ListModel[_], _ <: Integer]): Boolean = {
    val index = entry.getIdentifier
    val contact = entry.getValue(index).asInstanceOf[Delta]
    contact.name.contains(compare)
  }
}

class SearchFieldListener(val list: JXList) extends DocumentListener {

  def updateFilter(doc: Document): Unit = {
    val text = doc.getText(0, doc.getLength)
    list.setRowFilter(if (text.length > 0) new ContactRowFilter(text) else null)
  }

  override def insertUpdate(e: DocumentEvent): Unit = updateFilter(e.getDocument)

  override def changedUpdate(e: DocumentEvent): Unit = updateFilter(e.getDocument)

  override def removeUpdate(e: DocumentEvent): Unit = updateFilter(e.getDocument)
}