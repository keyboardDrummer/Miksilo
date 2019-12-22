package core.smarts

import core.smarts.language.Program
import core.smarts.language.expressions.Add
import core.smarts.language.modules.{Binding, Module}
import core.smarts.language.structs._
import core.smarts.language.types.{BoolType, IntType}
import org.scalatest.funsuite.AnyFunSuite

class UnionTypes extends AnyFunSuite with LanguageWriter {

  test("matchTypeZero") {
    val baseType = Struct("intOrBool", Seq.empty)
    val usage = Binding("main", MatchType(New("intOrBool", Seq()), Seq(TypeCase("intOrBool", "y", 1))))
    val module = Module("module", Seq(usage), Seq(baseType))
    Checker.check(Program(Seq(module)))
  }

  test("matchTypeSingle") {
    val baseType = Struct("intOrBool", Seq.empty)
    val iConstructor = Struct("i", Seq(Field("x", IntType)), maybeParent = Some("intOrBool"))
    val usage = Binding("main", MatchType(New("i", Seq("x" is 3)), Seq(TypeCase("i", "y", Add(2,Access("y","x"))))))
    val module = Module("module", Seq(usage), Seq(baseType, iConstructor))
    Checker.check(Program(Seq(module)))
  }

  test("basicMatchType") {
    val baseType = Struct("intOrBool", Seq.empty)
    val iConstructor = Struct("i", Seq(Field("x", IntType)), maybeParent = Some("intOrBool"))
    val bConstructor = Struct("b", Seq(Field("r", BoolType)), maybeParent = Some("intOrBool"))
    val usage = Binding("main", MatchType(New("intOrBool", Seq.empty), Seq(TypeCase("b", "z", 1), TypeCase("i", "y", Add(2,Access("y","x"))))))
    val module = Module("module", Seq(usage), Seq(baseType, iConstructor, bConstructor))
    Checker.check(Program(Seq(module)))
  }
}
