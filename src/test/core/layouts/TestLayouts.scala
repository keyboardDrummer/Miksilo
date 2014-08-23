package core.layouts

import org.junit.{Assert, Test}

class TestLayouts {

  val smallNumber: Double = 0.0001

  @Test
  def test() {
    val layout = new EquationLayout()
    val inputPanel = layout.createComponent
    val executeButton = layout.createComponent
    val outputPanel = layout.createComponent
    layout.expressions += inputPanel.width - outputPanel.width
    layout.addLeftToRight(layout.container, inputPanel, executeButton, outputPanel, layout.container)
    layout.expressions += executeButton.width - 100
    val solution = layout.solve(500, 500)
    Assert.assertEquals(0.0, solution(inputPanel.left), smallNumber)
    Assert.assertEquals(200, solution(inputPanel.right), smallNumber)
    Assert.assertEquals(300, solution(outputPanel.left), smallNumber)
    Assert.assertEquals(500, solution(outputPanel.right), smallNumber)
  }
}
