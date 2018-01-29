package core.nabl

import core.nabl.language.Program
import core.nabl.language.expressions._
import core.nabl.language.modules.{Binding, Module}
import core.nabl.language.structs._
import core.nabl.language.types.IntType
import core.nabl.modes.ConstraintClosure
import org.scalatest.FunSuite

class Structs extends FunSuite with LanguageWriter {


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
