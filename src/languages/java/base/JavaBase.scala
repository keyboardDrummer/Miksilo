package languages.java.base

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import scala.collection.mutable
import JavaBaseModel._
import languages.java.base.JavaMethodModel._
import languages.bytecode.{ByteCodeGoTo, ByteCode}
import languages.java.base.JavaTypes.{DoubleType, IntegerType}
import languages.java.base.ClassCompiler
import languages.java.base.MethodInfo
import languages.java.base.InstructionCompiler

class VariablePool {
  var offset = 0
  val variables = mutable.Map[String,Integer]()

  def add(variable: String, _type: Any) {
    variables(variable) = offset
    offset += JavaBase.getSize(_type)
  }
}

case class MethodInfo(location: Integer, method: MetaObject)

case class InstructionCompiler(classCompiler: ClassCompiler) {
  def findMethod(name: String, argumentTypes: Seq[MetaObject]) : MethodInfo = classCompiler.localStaticMethodRefLocations(name)
  def transformationState = classCompiler.transformationState
  val variables = new VariablePool()
  var localCount = 0
}

case class ClassCompiler(transformationState: TransformationState) {
  val localStaticMethodRefLocations = mutable.Map[String,MethodInfo]()
  val constantPool = new ConstantPool()
}


object JavaBase extends ProgramTransformation {


  def getSize(_type: Any): Int = _type match {
    case IntegerType => 1
    case DoubleType => 1
    case _ => throw new RuntimeException()
  }

  def getStatementToLines(state: TransformationState) = state.data.getOrElseUpdate(this, getInitialStatementToLines)
    .asInstanceOf[mutable.Map[AnyRef, (MetaObject, InstructionCompiler) => Seq[MetaObject]]]

  def getInitialStatementToLines = {
    val result = mutable.Map.empty[AnyRef, (MetaObject, InstructionCompiler) => Seq[MetaObject]]
    result.put(VariableKey, (variable, compiler) => {
      val variableAddress = compiler.variables.variables(getVariableName(variable))
      Seq(ByteCode.addressLoad(variableAddress))
    })
    result.put(CallKey, (call, compiler) => {
      val callCallee = getCallCallee(call)
      val callArguments = getCallArguments(call)
      val argumentInstructions = callArguments.flatMap(argument => statementToInstructions(argument, compiler))
      val methodInfo : MethodInfo = callCallee.clazz match {
        case VariableKey =>
          compiler.findMethod(getVariableName(callCallee), Seq())
        case SelectorKey => ???
      }
      argumentInstructions ++ Seq(ByteCode.invokeStatic(methodInfo.location))
    })
    result.put(Return, (_return,compiler) => {
      val returnValue = getReturnValue(_return)
      val returnValueInstructions = statementToInstructions(returnValue, compiler)
      returnValueInstructions ++ Seq(ByteCode.integerReturn)
    })
    result
  }

  def statementToInstructions(statement: MetaObject, instructionCompiler: InstructionCompiler)
    : Seq[MetaObject] = {
    val statementToSSMLines = getStatementToLines(instructionCompiler.transformationState)
    statementToSSMLines(statement.clazz)(statement, instructionCompiler)
  }

  def getMaxStack(instructions: Seq[MetaObject]): Integer = {
    var maxStack = 0
    var currentStack = 0
    for(instruction <- instructions) {
      currentStack += ByteCode.getInstructionStackSizeModification(instruction)
      maxStack = Math.max(maxStack, currentStack)
    }
    maxStack
  }

  def getLocalCount(instructions: Seq[MetaObject]): Integer = ???



  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      val className = JavaClassModel.getClassName(clazz)
      val classCompiler = new ClassCompiler(state)

      def getClassRef(name: String) = {
        val nameIndex = classCompiler.constantPool.store(name)
        classCompiler.constantPool.store(ByteCode.classRef(nameIndex))
      }

      val classRefIndex = getClassRef(className)
      clazz(ByteCode.ClassConstantPool) = classCompiler.constantPool.constants
      val methods = JavaClassModel.getMethods(clazz)
      for(method <- methods)
        bindMethod(method)
      
      for(method <- methods)
        convertMethod(method)


      def getMethodNameAndType(method: MetaObject): Int = {
        val methodNameIndex = getMethodNameIndex(method)
        val result: MetaObject = ByteCode.nameAndType(methodNameIndex, getMethodDescriptor(method))
        classCompiler.constantPool.store(result)
      }

      def getMethodRefIndex(method: MetaObject): Int = {
        val nameAndTypeIndex = getMethodNameAndType(method)
        classCompiler.constantPool.store(ByteCode.methodRef(classRefIndex, nameAndTypeIndex))
      }
      def bindMethod(method: MetaObject) = {
        val methodName = JavaMethodModel.getMethodName(method)
        val location: Int = getMethodRefIndex(method)
        classCompiler.localStaticMethodRefLocations(methodName) = new MethodInfo(location, method)
      }

      def addCodeAnnotation(method: MetaObject, parameters: Seq[MetaObject]) {
        val instructionCompiler = getInstructionCompiler(parameters)
        val statements = JavaMethodModel.getMethodBody(method)
        val instructions = statements.flatMap(statement => statementToInstructions(statement, instructionCompiler))
        val codeIndex = classCompiler.constantPool.storeUtf8("Code")
        val exceptionTable = Seq[MetaObject]()
        val codeAttributes = Seq[MetaObject]()
        val length = 0
        method(ByteCode.MethodAnnotations) = Seq(ByteCode.codeAttribute(codeIndex, length, getMaxStack(instructions), instructionCompiler.localCount,
          instructions, exceptionTable, codeAttributes))
      }

      def getInstructionCompiler(parameters: Seq[MetaObject]) = {
        val methodState = new InstructionCompiler(classCompiler)
        for (parameter <- parameters)
          methodState.variables.add(JavaMethodModel.getParameterName(parameter), JavaMethodModel.getParameterType(parameter))
        methodState
      }

      def getMethodNameIndex(method: MetaObject): Int = {
        val index = classCompiler.constantPool.storeUtf8(JavaMethodModel.getMethodName(method))
        index
      }
      def convertMethod(method: MetaObject) {
        val index: Int = getMethodNameIndex(method)
        method(ByteCode.MethodNameIndex) = index
        method.data.remove(JavaMethodModel.MethodNameKey)
        val parameters = JavaMethodModel.getMethodParameters(method)
        val methodDescriptorIndex = getMethodDescriptor(method)
        method(ByteCode.MethodDescriptorIndex) = methodDescriptorIndex
        method.data.remove(JavaMethodModel.ReturnTypeKey)
        addCodeAnnotation(method, parameters)

        method.data.remove(JavaMethodModel.MethodParametersKey)
      }

      def getMethodDescriptor(method: MetaObject): Int = {
        val returnType = JavaMethodModel.getMethodReturnType(method)
        val parameters = JavaMethodModel.getMethodParameters(method)
        val methodDescriptor = ByteCode.methodDescriptor(javaTypeToByteCodeType(returnType)
          , parameters.map(p => javaTypeToByteCodeType(JavaMethodModel.getParameterType(p))))
        classCompiler.constantPool.store(methodDescriptor)
      }

      def addMethodDescriptor(method: MetaObject) {
      }

    }
    def javaTypeToByteCodeType(_type: Any) : Any = {
      _type
    }


  }

  def dependencies: Set[ProgramTransformation] = Set(ByteCodeGoTo)
}
