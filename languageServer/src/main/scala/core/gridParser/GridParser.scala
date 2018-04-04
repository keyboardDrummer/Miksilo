package core.gridParser

import core.gridParser.grids.Grid

trait GridParser[T, R] {
  def parseInner(grid: Grid[T]): ParseResult[R]

  def parse(grid: Grid[T]): ParseResult[R] = {
    parseInner(grid) match {
      case success: ParseSuccess[R] => success
      case failure: ParseFailure[T] => ParseFailure(failure.message, failure.location + grid.origin)
    }
  }

  def name(name: String) = Named(name, this)
  def map[R2](f: R => R2): GridParser[T, R2] = MapParser(this, f)

  def ~[R2](right: => GridParser[T, R2]) = new LeftRight(this, right)
  def ~>[R2](right: => GridParser[T, R2]): GridParser[T, R2] = new LeftRight(this, right).map(r => r._2)
  def ~<[R2](right: => GridParser[T, R2]): GridParser[T, R] = new LeftRight(this, right).map(r => r._1)

  def %[R2](right: GridParser[T, R2]) = TopBottom(this, right)
  def |[R2](right: => GridParser[T, R]) = new OrParser(this, right)
  def indent(amount: Int = 2, canBeWider: Boolean = true): GridParser[T, R] = Indent(amount, canBeWider) ~> this

  def someVertical: GridParser[T, List[R]] = {
    (this % new ManyVertical[T, R](this)).map(t => t._1 :: t._2)
  }
}












