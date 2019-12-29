//package core.nabl
//
//
//import core.nabl.language.Program
//import core.nabl.language.expressions.Const
//import core.nabl.language.modules.{Binding, Module}
//import core.nabl.language.structs.{Field, New, Struct, StructFieldInit}
//import core.nabl.language.types.{LanguageTypeApplication, LanguageTypeVariable}
//import org.scalatest.funsuite.AnyFunSuite
//
//class PolymorphicStructs extends AnyFunSuite with LanguageWriter {
//
//  test("struct") {
//    val structDeclaration = Struct("s", Seq("x" of "a"), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq("x" is 3), genericTypeArgument = Some(IntType)))
//    val structUse = Binding("structUse", Variable("newStruct").access("x"), Some(IntType))
//    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.check(program)
//  }
//
//  test("structFail") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolType)),
//      Some(LanguageTypeApplication(new StructType("s"), IntType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
//    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  test("structFailWrongAccessType") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntType)),
//      Some(LanguageTypeApplication(new StructType("s"), IntType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(BoolType))
//    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  test("structFailMixedTypeVariablesInDeclaration") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("b"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntType)),
//      Some(LanguageTypeApplication(new StructType("s"), IntType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
//    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  test("structFailBadFieldInit") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", BoolConst(true))), Some(IntType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
//    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  test("reuseStruct") {
//    val structDeclaration = Struct("s", Seq(Field("x", _type = "a")), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", 3)), genericTypeArgument = Some(IntType)))
//    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", true)), genericTypeArgument = Some(BoolType)))
//    val structUse = Binding("structUse", Variable("newStruct").access("x"), bindingType = Some(IntType))
//    val structUse2 = Binding("structUse2", Variable("newStruct2").access("x"), bindingType = Some(BoolType))
//    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.check(program)
//  }
//
//  test("reuseStructFail") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntType)))
//    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", BoolConst(true))), Some(BoolType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(BoolType))
//    val structUse2 = Binding("structUse2", Access(Variable("newStruct2"), "x"), Some(BoolType))
//    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  test("reuseStructFail2") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolType)))
//    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", BoolConst(true))), Some(BoolType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
//    val structUse2 = Binding("structUse2", Access(Variable("newStruct2"), "x"), Some(BoolType))
//    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  test("structFailBadFieldInit2") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(BoolType))
//    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  test("reuseStructFailBadFieldInit") {
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntType)))
//    val structNew2 = Binding("newStruct2", New("s", Seq(StructFieldInit("x", Const(3))), Some(BoolType)))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
//    val structUse2 = Binding("structUse2", Access(Variable("newStruct2"), "x"), Some(BoolType))
//    val module = Module("module", Seq(structNew, structNew2, structUse2, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.fail(program)
//  }
//
//  ignore("structNewWithStaticType") {
//    val structNew = Binding("newStruct", New("s", Seq(StructFieldInit("x", Const(3))), Some(IntType)),
//      Some(LanguageTypeApplication(new StructType("s"), IntType)))
//    val structDeclaration = Struct("s", Seq(Field("x", LanguageTypeVariable("a"))), typeParameter = Some("a"))
//    val structUse = Binding("structUse", Access(Variable("newStruct"), "x"), Some(IntType))
//    val module = Module("module", Seq(structNew, structUse), Seq(structDeclaration))
//    val program: Program = Program(Seq(module))
//    Checker.check(program)
//  }
//
//  test("list with map") {
//    val listDef = Struct("list", Seq("head" of "a", "tail" of LanguageTypeApplication("list", LanguageTypeVariable("a"))))
//    val program = Program(Seq(Module("module", Seq.empty, Seq(listDef))))
//    Checker.check(program)
//  }
//}
