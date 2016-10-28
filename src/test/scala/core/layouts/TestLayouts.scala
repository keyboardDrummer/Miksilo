package core.layouts

import org.junit.{Assert, Test}
import org.scalatest.FunSuite

class TestLayouts extends FunSuite {

  val smallNumber: Double = 0.0001

  def test() {
    val layout = new EquationLayout()
    val inputPanel = layout.createComponent
    val executeButton = layout.createComponent
    val outputPanel = layout.createComponent
    layout.expressions += inputPanel.width - outputPanel.width
    layout.addLeftToRight(layout.container, inputPanel, executeButton, outputPanel, layout.container)
    layout.expressions += executeButton.width - 100

    layout.addEquals(layout.container.top, inputPanel.top, executeButton.top, outputPanel.top)
    layout.addEquals(layout.container.bottom, inputPanel.bottom, executeButton.bottom, outputPanel.bottom)
    val solution = layout.solve(500, 500)
    assertResult(0.0)( solution(inputPanel.left), smallNumber)
    assertResult(200)( solution(inputPanel.right), smallNumber)
    assertResult(300)( solution(outputPanel.left), smallNumber)
    assertResult(500)( solution(outputPanel.right), smallNumber)
  }
}
