package core.gridParser

object Size{
  val zero = Size(0,0)
}
case class Size(width: Int, height: Int) {
  def +(other: Size) = Size(width + other.width, height + other.height)
}
