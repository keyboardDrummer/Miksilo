package core.bigrammar

import core.bigrammar.grammars._
import core.document.BlankLine

import scala.language.higherKinds
import scala.reflect.ClassTag

trait AbstractGrammarWriter[Grammar[_]] extends BiGrammarWriter {

  def map2[Value, NewValue: ClassTag](grammar: Grammar[Value],
                                     afterParsing: Value => NewValue, beforePrinting: NewValue => Option[Value]): Grammar[NewValue]

  def choice2[Value](grammar: Grammar[Value],
                    other: Grammar[Value], firstIsLonger: Boolean): Grammar[Value]

  def sequence2[Value, Other, Result](grammar: Grammar[Value],
                                     other: Grammar[Other],
                                     bijective: SequenceBijective[Value, Other, Result],
                                     horizontal: Boolean): Grammar[Result]

  def many2[Value](grammar: Grammar[Value], horizontal: Boolean): Grammar[List[Value]]

  implicit def stringToGrammar(value: String): Grammar[Unit]
  implicit def wrapBiGrammar[T](grammar: BiGrammar[T]): Grammar[T]

  def listConsBijective[Value] = new SequenceBijective[Value, List[Value], List[Value]]((h, t) => h :: t, {
    case head :: tail => Some(head -> tail)
    case Nil => None
  })

  implicit class ValueGrammarExtension[Value](grammar: Grammar[Value]) {
    def map[NewValue: ClassTag](afterParsing: Value => NewValue, beforePrinting: NewValue => Option[Value]): Grammar[NewValue] =
      AbstractGrammarWriter.this.map2(grammar, afterParsing, beforePrinting)

    def choice(other: Grammar[Value], firstIsLonger: Boolean): Grammar[Value] =
      AbstractGrammarWriter.this.choice2(grammar, other, firstIsLonger)

    def option: Grammar[Option[Value]] =
      new ValueGrammarExtension(map[Option[Value]](x => Some(x), x => x)).choice(wrapBiGrammar(value(None)), firstIsLonger = true)

    def sequence[Other, Result](other: Grammar[Other],
                                bijective: SequenceBijective[Value, Other, Result],
                                horizontal: Boolean): Grammar[Result] =
      sequence2(grammar, other, bijective, horizontal)

    def many(horizontal: Boolean = false): Grammar[List[Value]] = many2(grammar, horizontal)

    def ~[Right](other: Grammar[Right]): Grammar[(Value, Right)] = sequence(other, BiSequence.tuple, horizontal = true)
    def %[Bottom](other: Grammar[Bottom]): Grammar[(Value, Bottom)] = sequence(other, BiSequence.tuple, horizontal = false)
    def ~<(right: Grammar[Unit]): Grammar[Value] = sequence[Unit, Value](right, BiSequence.ignoreRight[Value], horizontal = true)
    def %<(bottom: Grammar[Unit]): Grammar[Value] = sequence[Unit, Value](bottom, BiSequence.ignoreRight, horizontal = false)

    def manyVertical: Grammar[List[Value]] = many()
    def ~~<(right: Grammar[Unit]): Grammar[Value] = this ~< printSpace ~< right

    def manySeparated(separator: Grammar[Unit], horizontal: Boolean = true): Grammar[List[Value]] = {
      new ValueGrammarExtension(someSeparated(separator, horizontal)).choice(ValueGrammar(List.empty[Value]), firstIsLonger = true)
    }

    def ~~[Right](right: Grammar[Right]): Grammar[(Value, Right)] = {
      grammar ~< printSpace ~ right
    }

    def someSeparatedVertical(separator: Grammar[Unit]): Grammar[List[Value]] = someSeparated(separator, horizontal = false)

    def manySeparatedVertical(separator: Grammar[Unit]): Grammar[List[Value]] = manySeparated(separator, horizontal = false)

    def some(horizontal: Boolean = true): Grammar[List[Value]] = grammar.sequence(grammar*, listConsBijective[Value], horizontal)
    def someSeparated(separator: Grammar[Unit], horizontal: Boolean): Grammar[List[Value]] =
      this.sequence((separator ~> grammar).*, listConsBijective, horizontal)

    def inParenthesis: Grammar[Value] = stringToGrammar("(") ~> grammar ~< ")"
    def inBraces: Grammar[Value] = stringToGrammar("{") ~> grammar ~< "}"

    def * : Grammar[List[Value]] = many()

    def %%[Bottom](bottom: Grammar[Bottom]): Grammar[(Value, Bottom)] = {
      this %< print(BlankLine) % bottom
    }

    def spacedOption: Grammar[Option[Value]] = (wrapBiGrammar(printSpace) ~> grammar).option
    def toParameterList: Grammar[List[Value]] = {
      new ValueGrammarExtension(grammar.manySeparated(stringToGrammar(",") ~< printSpace)).inParenthesis
    }
  }

  implicit class UnitGrammarExtension(grammar: Grammar[Unit]) extends BiGrammarWriter {

    def ~>[Right](right: Grammar[Right]): Grammar[Right] = grammar.sequence[Right, Right](right, BiSequence.ignoreLeft[Right], horizontal = true)

    def ~[Right](right: Grammar[Right]): Grammar[Right] = grammar.sequence[Right, Right](right, BiSequence.ignoreLeft[Right], horizontal = true)

    def %[Bottom](bottom: Grammar[Bottom]): Grammar[Bottom] = grammar.sequence[Bottom, Bottom](bottom, BiSequence.ignoreLeft[Bottom], horizontal = false)

    //def %>[Bottom](bottom: Grammar[Bottom]): Grammar[Bottom] = grammar.sequence[Bottom, Bottom](bottom, BiSequence.ignoreLeft[Bottom], horizontal = false)

    def ~~[Right](right: Grammar[Right]): Grammar[Right] = grammar ~> printSpace ~> right
  }
}

