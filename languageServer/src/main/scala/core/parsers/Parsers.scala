package core.parsers

// TODO misschien proberen door te parsen wanneer er een failure plaatsvindt, zodat ik bij 'missende input' gewoon verder ga. Ik zou ook nog recovery parsers kunnen toevoegen zoals in de paper, maar dat lijkt overkil.

trait Parsers extends Processors {

  type ProcessResult[+R] = ParseResult[Input, R]
  type PF[+R] = ParseFailure[Input, R]
  type PR[+R] = ParseResult[Input, R]
  type PS[+R] = ParseSuccess[Input, R]

  override def leftRight[Left, Right, NewResult](left: Processor[Left], right: => Processor[Right], combine: (Left, Right) => NewResult) =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR) = ???

  override def choice[Result](first: Processor[Result], other: => Processor[Result], leftIsAlwaysBigger: Boolean) =
    if (leftIsAlwaysBigger) new OrElse(first, other) else new BiggestOfTwo(first, other)

  override def flatMap[Result, NewResult](left: Processor[Result], f: Result => Processor[NewResult]) = ???

  trait Parser[+Result] extends Processor[Result] {

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(this, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): Parser[Other] =
      WithDefault[Other](this, cache => Some(_default))

    //  def addAlternative[Other >: Result](getAlternative: (Parser[Other], Parser[Other]) => Parser[Other]): Parser[Other] = {
    //    lazy val result: Parser[Other] = new Lazy(this ||| getAlternative(this, result))
    //    result
    //  }

    //  def ~[Right](right: => Parser[Right]) = new Sequence(this, right, (a: Result, b: Right) => (a,b))
    //  def ~<[Right](right: Parser[Right]) = new Sequence(this, right, (a: Result, _: Right) => a)
    //  def ~>[Right](right: Parser[Right]) = new Sequence(this, right, (_: Result, b: Right) => b)
    //  def |[Other >: Result](other: => Parser[Other]) = new OrElse[Input, Result, Other, Other](this, other)
    //  def |||[Other >: Result](other: => Parser[Other]) = new BiggestOfTwo[Input, Result, Other, Other](this, other)
    //  def map[NewResult](f: Result => NewResult) = new MapParser(this, f)
    //  def option: Parser[Option[Result]] = this.map(x => Some(x)) | Return[Input, Option[Result]](None)
    //  def flatMap[NewResult](f: Result => Parser[NewResult]) = new FlatMap(this, f)
    //  def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(this, predicate, getMessage)
    //  def withDefault[Other >: Result](_default: Other): Parser[Other] = WithDefault[Input, Other](this, cache => Some(_default))

    override def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum): Parser[Sum] = {
      lazy val result: Parser[Sum] = leftRight(this, result, reduce).withDefault[Sum](zero) | (succeed(zero), true)
      result
    }

    //  def * : Parser[List[Result]] = {
    //    many(List.empty, (h: Result, t: List[Result]) => h :: t)
    //  }
    //
    //  def ^^[NewResult](f: Result => NewResult) = new MapParser(this, f)
    //  def manySeparated(separator: Parser[Any]): Parser[List[Result]] =
    //    new Sequence(this, (separator ~> this)*, (h: Result, t: List[Result]) => h :: t) |
    //      Return(List.empty)

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): Parser[Other] = {
      val withPosition = new Sequence(
        new PositionParser[Input](),
        new WithRemainderParser(this),
        (left: Input, resultRight: (Result, Input)) => addRange(left, resultRight._2, resultRight._1))
      WithDefault(withPosition, cache => this.getDefault(cache))
    }
  }

  case class WithDefault[+Result](original: Parser[Result], _getDefault: DefaultCache => Option[Result])
    extends Parser[Result] {
    override def parseNaively(input: Input, state: ParseState): PR[Result] = {
      original.parseCached(input, state) match {
        case failure: PF[Result] if failure.partialResult.isEmpty =>
          new PF[Result](_getDefault(state.defaultCache), failure.remainder, failure.message)
        case x => x
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      _getDefault(cache)
  }

  class Sequence[+Left, +Right, +Result](left: Processor[Left],
                                         _right: => Processor[Right],
                                         combine: (Left, Right) => Result) extends Parser[Result] {
    lazy val right: Processor[Right] = _right

    override def parseNaively(input: Input, state: ParseState): PR[Result] = {
      val leftResult = left.parseCached(input, state)
      leftResult match {
        case leftSuccess: PS[Left] =>
          val rightResult = right.parseCached(leftSuccess.remainder, state)
          rightResult match {
            case rightSuccess: PS[Right] =>
              rightSuccess.map(r => combine(leftSuccess.result, r)).
                addFailure(leftSuccess.biggestFailure.map(l => combine(l, rightSuccess.result)))

            case rightFailure: PF[Right] =>
              if (leftSuccess.biggestFailure.offset > rightFailure.offset && rightFailure.partialResult.nonEmpty) {
                val rightDefault = rightFailure.partialResult.get
                leftSuccess.biggestFailure.map(l => combine(l, rightDefault)).asInstanceOf[PF[Result]]
              }
              else {
                rightFailure.map(right => combine(leftSuccess.result, right))
              }
          }

        case leftFailure: PF[Left] =>
          val result = for {
            leftPartial <- leftFailure.partialResult
            rightDefault <- right.getDefault(state)
          } yield combine(leftPartial, rightDefault)
          ParseFailure(result, leftFailure.remainder, leftFailure.message)
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(right)
    } yield combine(leftDefault, rightDefault)
  }

  class OrElse[+First <: Result, +Second <: Result, +Result](first: Processor[First], _second: => Processor[Second])
    extends Processor[Result] {
    lazy val second = _second

    override def parseNaively(input: Input, cache: ParseState): PR[Result] = {
      val firstResult = first.parseCached(input, cache)
      firstResult match {
        case _: PS[Result] => firstResult
        case firstFailure: PF[Result] =>
          val secondResult = second.parseCached(input, cache)
          secondResult match {
            case secondSuccess: PS[Result] =>
              val biggestFailure = firstFailure.getBiggest(secondSuccess.biggestFailure)
              ParseSuccess(secondSuccess.result, secondSuccess.remainder, biggestFailure)
            case secondFailure: PF[Result] =>
              firstFailure.getBiggest(secondFailure)
          }
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  class BiggestOfTwo[+First <: Result, +Second <: Result, +Result](first: Processor[First], _second: => Processor[Second])
    extends Parser[Result] {
    lazy val second = _second

    override def parseNaively(input: Input, state: ParseState): PR[Result] = {
      val firstResult = first.parseCached(input, state)
      val secondResult = second.parseCached(input, state)
      (firstResult, secondResult) match {
        case (firstSuccess: PS[Result], secondSuccess: PS[Result]) =>
          if (firstSuccess.remainder.offset > secondSuccess.remainder.offset)
            firstSuccess.addFailure(secondSuccess.biggestFailure)
          else
            secondSuccess.addFailure(firstSuccess.biggestFailure)
        case (firstFailure: PF[Result], secondSuccess: PS[Result]) =>
          secondSuccess.addFailure(firstFailure)
        case (firstSuccess: PS[Result], secondFailure: PF[Result]) =>
          firstSuccess.addFailure(secondFailure)
        case (firstFailure: PF[Result], secondFailure: PF[Result]) =>
          firstFailure.getBiggest(secondFailure)
        case _ => throw new Exception("can not occur")
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }
}