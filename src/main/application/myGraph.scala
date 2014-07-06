package application

import com.mxgraph.view.mxGraph
import com.mxgraph.model.mxCell

class myGraph extends mxGraph {

  override def isCellMovable(cell: scala.Any): Boolean = {
    cell match {
      case myCell: mxCell if myCell.isEdge => false
      case _ => super.isCellEditable(cell)
    }
  }

  override def isCellEditable(cell: scala.Any): Boolean = {
    cell match {
      case myCell: mxCell if myCell.isEdge => false
      case _ => super.isCellEditable(cell)
    }
  }
}
