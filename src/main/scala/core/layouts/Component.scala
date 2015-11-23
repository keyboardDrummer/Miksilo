package core.layouts

trait ComponentProperty

object Top extends ComponentProperty {
  override def toString = "Top"
}

object Bottom extends ComponentProperty {
  override def toString = "Bottom"
}

object Left extends ComponentProperty {
  override def toString = "Left"
}

object Right extends ComponentProperty {
  override def toString = "Right"
}

case class Component(index: Int) {
  def top = new Variable(this, Top)

  def bottom = new Variable(this, Bottom)

  def left = new Variable(this, Left)

  def right = new Variable(this, Right)

  def height = bottom - top

  def width: Expression = right - left

  def horizontalCenter2 = left + right
  def verticalCenter2 = top + bottom
}