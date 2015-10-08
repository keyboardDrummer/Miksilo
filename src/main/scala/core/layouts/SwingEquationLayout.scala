package core.layouts

import java.awt
import java.awt.{Container, Dimension, LayoutManager}

class SwingEquationLayout(val container: Container) {
  val equationLayout = new EquationLayout()
  var componentMapping: Map[java.awt.Component, Component] = Map.empty
  var componentMappingReverse: Map[Component, java.awt.Component] = Map.empty
  container.setLayout(toLayoutManager)

  def expressions = equationLayout.expressions

  def expressions_=(value: Set[Expression]) = equationLayout.expressions = value

  def addComponent(comp: awt.Component): Component = {
    val result = equationLayout.createComponent
    container.add(comp)
    componentMapping += comp -> result
    componentMappingReverse += result -> comp
    result
  }

  def makePreferredWidth(component: Component) = {
    val swingComponent = componentMappingReverse(component)
    equationLayout.expressions ++= Seq(component.width - swingComponent.getPreferredSize.width)
  }

  def makePreferredSize(component: Component) = {
    val swingComponent = componentMappingReverse(component)
    equationLayout.expressions ++= Seq(component.height - swingComponent.getPreferredSize.height,
      component.width - swingComponent.getPreferredSize.width)
  }

  def performLayout(): Unit = {
    val solution = equationLayout.solve(container.getWidth, container.getHeight)
    for (swingComponent <- componentMapping.keys) {
      val equationComponent = componentMapping(swingComponent)
      val left = solution(equationComponent.left).toInt
      val right = solution(equationComponent.right).toInt
      val top = solution(equationComponent.top).toInt
      val bottom = solution(equationComponent.bottom).toInt
      val width = right - left
      val height = bottom - top
      swingComponent.setBounds(left, top, width, height)
    }
  }

  def toLayoutManager = new LayoutManager {
    override def layoutContainer(parent: Container): Unit = performLayout()

    override def removeLayoutComponent(comp: awt.Component): Unit = throw new UnsupportedOperationException

    override def addLayoutComponent(name: String, comp: awt.Component): Unit = throw new UnsupportedOperationException

    override def preferredLayoutSize(parent: Container): Dimension = new Dimension(1000, 1000)

    override def minimumLayoutSize(parent: Container): Dimension = new Dimension(500, 500)
  }
}
