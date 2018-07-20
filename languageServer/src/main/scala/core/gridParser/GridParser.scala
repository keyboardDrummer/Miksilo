package core.gridParser

import core.gridParser.grids.Grid

trait GridParser[T, R] {

  def parseEntireGrid(grid: Grid[T]): ParseResult[R] = {
    parse(grid).flatMap(success => {
      if (success.size == grid.size) {
        return ParseSuccess(success.size, success.result)
      }
      success.biggestFailure.get
    })
  }

  def parse(grid: Grid[T]): ParseResult[R]

  def name(name: String) = Named(name, this)
  def map[R2](f: R => R2): GridParser[T, R2] = MapParser(this, f)

  def ~[R2](right: => GridParser[T, R2]) = new LeftRight(this, right)
  def ~>[R2](right: => GridParser[T, R2]): GridParser[T, R2] = new LeftRight(this, right).map(r => r._2)
  def ~<[R2](right: => GridParser[T, R2]): GridParser[T, R] = new LeftRight(this, right).map(r => r._1)

  def %[R2](right: GridParser[T, R2]) = TopBottom(this, right)
  def %<[R2](bottom: GridParser[T, R2]) = (this % bottom).map(r => r._1)
  def |[R2](right: => GridParser[T, R]) = new OrParser(this, right)
  def indent(amount: Int = 2, canBeWider: Boolean = true): GridParser[T, R] = Indent(amount, canBeWider, mustParseSomething = true) ~> this

  def someVertical: GridParser[T, List[R]] = {
    (this % new ManyVertical[T, R](this)).map(t => t._1 :: t._2)
  }
}












