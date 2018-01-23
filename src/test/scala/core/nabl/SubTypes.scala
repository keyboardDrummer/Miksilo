package core.nabl


import core.nabl.language.Program
import core.nabl.language.expressions._
import core.nabl.language.modules.{Binding, Module}
import core.nabl.language.structs._
import core.nabl.language.types._
import org.scalatest.FunSuite

class SubTypes extends FunSuite with LanguageWriter {

  test("intAddition") {
    val program = OverloadedAdd(Const(3), Const(2))
    Checker.checkExpression(program, IntType)
  }

  test("longAddition") {
    val program = OverloadedAdd(LongConst(3), LongConst(2))
    Checker.checkExpression(program, LongType)
  }

  test("struct") {
    val structParent = Struct("s", Seq(Field("x", IntType)))
    val structChild = Struct("s2", Seq(), Some("s"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("s2")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structFail") {
    val structParent = Struct("s", Seq(Field("x", IntType)))
    val structChild = Struct("s2", Seq(), Some("s3"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("s2")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structFail2") {
    val structParent = Struct("s", Seq(Field("y", IntType)))
    val structChild = Struct("s2", Seq(), Some("s"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("s2")))
    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("structBigger") {
    val structParent = Struct("s", Seq(Field("x", IntType)))
    val structChild = Struct("s2", Seq(Field("y", IntType)), Some("s"))
    val structNew = Binding("newStruct", New("s2", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new StructType("s2")))
    val structUse = Binding("structUse", Add(Access(Variable("newStruct"), "x"), Access(Variable("newStruct"), "y")), Some(IntType))
    val module = Module("module", Seq(structNew, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("structBiggerFail") {
    val structParent = Struct("s", Seq(Field("x", IntType)))
    val structChild = Struct("s2", Seq(Field("y", IntType)), Some("s"))
    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new StructType("s")))
    val module = Module("module", Seq(structNew), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("longLambdaTakesInt") {
    val program = Application(Lambda("x", Variable("x"), Some(LongType)), Const(3))
    Checker.checkExpression(program, IntType)
  }

  test("longLambdaTakesIntFail") {
    val program = Application(Lambda("x", Variable("x"), Some(LongType)), Const(3))
    Checker.failExpression(program, LongType)
  }

  test("intLambdaTakesLong") {
    val program = Application(Lambda("x", Variable("x"), Some(IntType)), LongConst(3))
    Checker.failExpression(program)
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismLambda") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq(), Some("s"))
    val takesSuperStruct = Lambda("struct", Const(3), Some(new StructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismLambdaFail") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq())
    val takesSuperStruct = Lambda("struct", Const(3), Some(new StructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("lambdaTakingChildStructSimpleLambda") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq(), Some("s"))
    val takesSuperStruct = Lambda("struct", Const(3), Some(new StructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("lambdaTakingChildStructSimpleNoPolymorphismContraVariantApplicationFail") {
    val structParent = Struct("s", Seq())
    val structChild = Struct("s2", Seq())
    val takesSuperStruct = Lambda("struct", Const(3), Some(new StructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), New("s2", Seq.empty))), Some(IntType))
    val module = Module("module", Seq(structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.fail(program)
  }

  test("lambdaTakingChildStructLambda") {
    val structParent = Struct("s", Seq(Field("x", IntType)))
    val structChild = Struct("s2", Seq(Field("y", IntType)), Some("s"))
    val newChild = Binding("newChild", New("s2", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new StructType("s2")))
    val takesSuperStruct = Lambda("struct", Access(Variable("struct"), "x"), Some(new StructType("s")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Application(Variable("takesSuperStruct"), Variable("newChild"))), Some(IntType))
    val module = Module("module", Seq(newChild, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("lambdaTakingParentAndChildStructLambda") {
    val structParent = Struct("parent", Seq(Field("x", IntType)))
    val structChild = Struct("child", Seq(Field("y", IntType)), Some("parent"))
    val newParent = Binding("newParent", New("parent", Seq(StructFieldInit("x", Const(3)))), Some(new StructType("parent")))
    val newChild = Binding("newChild", New("child", Seq(StructFieldInit("x", Const(3)), StructFieldInit("y", Const(2)))), Some(new StructType("child")))
    val takesSuperStruct = Lambda("struct", Access(Variable("struct"), "x"), Some(new StructType("parent")))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Add(
        Application(Variable("takesSuperStruct"), Variable("newChild")),
        Application(Variable("takesSuperStruct"), Variable("newParent")))),
      Some(IntType))
    val module = Module("module", Seq(newChild, newParent, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("genericLambdaTakingParentAndChildStruct") {
    val structParent = Struct("parent", Seq("x" of IntType))
    val structChild = Struct("child", Seq("y" of IntType), Some("parent"))
    val newChild = Binding("newChild", New("child", Seq("x" is 3, "y" is 2)), Some(new StructType("child")))
    val newParent = Binding("newParent", New("parent", Seq("x" is 3)), Some(new StructType("parent")))
    val takesSuperStruct = Lambda("struct", Variable("struct").access("x"))
    val structUse = Binding("structUse", Let("takesSuperStruct", takesSuperStruct,
      Add(Variable("takesSuperStruct") $ "newChild",
          Variable("takesSuperStruct") $ "newParent")), Some(IntType))
    val module = Module("module", Seq(newChild, newParent, structUse), Seq(structParent, structChild))
    val program: Program = Program(Seq(module))
    Checker.check(program)
  }

  test("polymorphic function with subtype constraint complex") {
    val function = Lambda("x", "x", Some(LongType))
    val const = Lambda("x", Lambda("y", "x"))
    val program = Let("f", function, const $ (Variable("f") $ Const(3)) $ (Variable("f") $ LongConst(3)))
    Checker.checkExpression(program)
  }

  test("polymorphic function with subtype constraint fail") {
    val program = Lambda("x", "x", Some(LongType)) $ true
    Checker.failExpression(program, BoolType)
  }
}
