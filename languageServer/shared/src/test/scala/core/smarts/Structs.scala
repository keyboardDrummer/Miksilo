package core.smarts

import core.smarts.language.Program
import core.smarts.language.expressions._
import core.smarts.language.modules.{Binding, Module}
import core.smarts.language.structs._
import core.smarts.language.types.IntType
import core.smarts.modes.ConstraintClosure
import org.scalatest.funsuite.AnyFunSuite

class Structs extends AnyFunSuite with LanguageWriter {


  test("struct") {
    val structDeclaration = Struct("s", Seq(Field("x", IntType)))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structFail") {
    val structDeclaration = Struct("s", Seq(Field("x", IntType)))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFail2") {
    val structDeclaration = Struct("s", Seq(Field("x", IntType)))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct2"), "x"), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFailBadFieldInit") {
    val structDeclaration = Struct("s", Seq(Field("x", IntType)))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", BoolConst(true)))), Some(new StructType("s")))
    val structUse = Binding("structUse", Access(Variable("newStruct2"), "x"), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("lambdaTakingStruct") {
    val structDeclaration = Struct("s", Seq(Field("x", IntType)))
    val newStruct = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("s")))
    val takesStruct = Lambda("struct", Access(Variable("struct"), "x"), Some(new StructType("s")))
    val structUse = Binding("structUse", Let("takesStruct", takesStruct,
      Application(Variable("takesStruct"), Variable("newStruct"))), Some(IntType))
    val module = Module("module", Seq(newStruct, structUse), Seq(structDeclaration))
    val program: Program = Program(Seq(module))
    ConstraintClosure.succeed(program)
  }

  test("intList") {
    val listDef = Struct("intList", Seq("head" of IntType, "tail" of "intList"))
    val program = Program(Seq(Module("module", Seq.empty, Seq(listDef))))
    Checker.check(program)
  }

  ignore("intList fail") {
    val listDef = Struct("intList", Seq("head" of IntType, "tail" of "intList2"))
    val program = Program(Seq(Module("module", Seq.empty, Seq(listDef))))
    Checker.fail(program)
  }
}
