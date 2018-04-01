package core.gridParser

import core.gridParser.grids.Grid

import scala.util.parsing.combinator.JavaTokenParsers

trait ParseResult[R] {
  def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2]
  def map[R2](mapping: R => R2): ParseResult[R2]
}

case class ParseSuccess[R](size: Size, result: R) extends ParseResult[R] {
  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = {
    next(this)
  }

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseSuccess(size, mapping(result))
}

case class ParseFailure[R](message: String, location: Location) extends ParseResult[R] {
  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = new ParseFailure[R2](message, location)

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseFailure(message, location)
}

case class Succeed[T, R](value: R) extends GridParser[T, R] {
  override def parseInner(grid: Grid[T]): ParseResult[R] = ParseSuccess(Size.zero, value)
}

class Fail[T, R](message: String) extends GridParser[T, R] {
  override def parseInner(grid: Grid[T]): ParseResult[R] = {
    ParseFailure(message, Location.zero)
  }
}

case class Named[T, R](name: String, inner: GridParser[T, R]) extends GridParser[T, R] {
  override def parseInner(grid: Grid[T]): ParseResult[R] = inner.parseInner(grid)

  override def toString: String = name
}

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

case class MapParser[T, R, R2](parser: GridParser[T, R], mapping: R => R2) extends GridParser[T, R2] {
  override def parseInner(grid: Grid[T]): ParseResult[R2] = parser.parseInner(grid).map(mapping)
}

case class ManyVertical[T, R](parser: GridParser[T, R]) extends GridParser[T, List[R]] {
  private val topBottom = new TopBottom[T, R, List[R]](parser, this).map(t => t._1 :: t._2)
  private val result = new OrParser[T, List[R]](topBottom, Succeed[T, List[R]](List.empty))

  override def parseInner(grid: Grid[T]): ParseResult[List[R]] = {
    result.parseInner(grid)
  }
}

object CharParsers extends JavaTokenParsers {}












