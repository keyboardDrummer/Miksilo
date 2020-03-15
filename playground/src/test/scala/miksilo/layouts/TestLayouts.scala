package miksilo.core.layouts

import org.scalatest.funsuite.AnyFunSuite

class TestLayouts extends AnyFunSuite {

  val smallNumber: Double = 0.0001

  test("basic") {
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
    assert(solution(inputPanel.left) < smallNumber)
    assert(Math.abs(200 - solution(inputPanel.right)) < smallNumber)
    assert(Math.abs(300 - solution(outputPanel.left)) < smallNumber)
    assert(Math.abs(500 - solution(outputPanel.right)) < smallNumber)
  }
}
