package languages.javac.base

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import scala.collection.mutable
import JavaBaseModel._
import languages.javac.base.JavaMethodModel._
import languages.bytecode.{ByteCodeGoTo, ByteCode}
import languages.javac.base.JavaTypes.{DoubleType, IntegerType}
import languages.javac.base.{MethodInfo, MethodKey, ClassCompiler}

class VariablePool {
  var offset = 0
  val variables = mutable.Map[String,Integer]()

  def add(variable: String, _type: Any) {
    variables(variable) = offset
    offset += JavaBase.getSize(_type)
  }
}


case class InstructionCompiler(classCompiler: ClassCompiler) {
  def transformationState = classCompiler.transformationState
  val variables = new VariablePool()
  var localCount = 0
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
      Seq(ByteCode.integerLoad(variableAddress))
    })

    result.put(CallKey, (call, compiler) => {
      val callCallee = getCallCallee(call)
      val methodKey : MethodKey = callCallee.clazz match {
        case VariableKey =>
          new MethodKey(getQualifiedClassName(compiler.classCompiler.currentClass), getVariableName(callCallee))
        case SelectorKey => ???
      }
      val callArguments = getCallArguments(call)
      val argumentInstructions = callArguments.flatMap(argument => statementToInstructions(argument, compiler))
      val methodRefIndex = compiler.classCompiler.getMethodRefIndex(methodKey)
      argumentInstructions ++ Seq(ByteCode.invokeStatic(methodRefIndex))
    })

    result.put(Return, (_return,compiler) => {
      val mbValue = getReturnValue(_return)
      mbValue match {
        case Some(value) =>
          val returnValueInstructions = statementToInstructions(value, compiler)
          returnValueInstructions ++ Seq(ByteCode.integerReturn)
        case None => Seq(ByteCode.voidReturn)
      }
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

  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      val qualifiedClassName: QualifiedClassName = getQualifiedClassName(clazz)
      val className = JavaClassModel.getClassName(clazz)
      val classCompiler = new ClassCompiler(clazz, state)
      val myPackage = classCompiler.compiler.getPackage(JavaClassModel.getPackage(clazz).toList)
      val classInfo = new ClassInfo()
      myPackage.content(className) = classInfo

      classCompiler.getClassRef(qualifiedClassName) //TODO Just to store the classRef. I don't think this is really necessary but Javac also generates this classRef in the constantpool.
      clazz(ByteCode.ClassConstantPool) = classCompiler.constantPool.constants
      val methods = JavaClassModel.getMethods(clazz)
      for(method <- methods)
        bindMethod(method)
      
      for(method <- methods)
        convertMethod(method)

      def bindMethod(method: MetaObject) = {
        val methodName: String = JavaMethodModel.getMethodName(method)
        val descriptor = getMethodDescriptor(method)
        classInfo.content(methodName) = new MethodInfo(descriptor)
      }

      def addCodeAnnotation(method: MetaObject, parameters: Seq[MetaObject]) {
        val instructionCompiler = getInstructionCompiler(parameters)
        val statements = JavaMethodModel.getMethodBody(method)
        val instructions = statements.flatMap(statement => statementToInstructions(statement, instructionCompiler))
        val codeIndex = classCompiler.constantPool.store(ByteCode.CodeAttributeId)
        val exceptionTable = Seq[MetaObject]()
        val codeAttributes = Seq[MetaObject]()
        method(ByteCode.MethodAnnotations) = Seq(ByteCode.codeAttribute(codeIndex, getMaxStack(instructions), instructionCompiler.localCount,
          instructions, exceptionTable, codeAttributes))
      }

      def getInstructionCompiler(parameters: Seq[MetaObject]) = {
        val methodState = new InstructionCompiler(classCompiler)
        for (parameter <- parameters)
          methodState.variables.add(JavaMethodModel.getParameterName(parameter), JavaMethodModel.getParameterType(parameter))
        methodState
      }

      def convertMethod(method: MetaObject) {
        val methodNameIndex: Int = classCompiler.getMethodNameIndex(JavaMethodModel.getMethodName(method))
        method(ByteCode.MethodNameIndex) = methodNameIndex
        method.data.remove(JavaMethodModel.MethodNameKey)
        val methodDescriptorIndex = getMethodDescriptorIndex(method)
        method(ByteCode.MethodDescriptorIndex) = methodDescriptorIndex
        val parameters = JavaMethodModel.getMethodParameters(method)
        addCodeAnnotation(method, parameters)
        method.data.remove(JavaMethodModel.ReturnTypeKey)
        method.data.remove(JavaMethodModel.MethodParametersKey)
      }

      def getMethodDescriptorIndex(method: MetaObject) : Int = classCompiler.constantPool.store(getMethodDescriptor(method))
      def getMethodDescriptor(method: MetaObject) : MetaObject = {
        val returnType = JavaMethodModel.getMethodReturnType(method)
        val parameters = JavaMethodModel.getMethodParameters(method)
        ByteCode.methodDescriptor(javaTypeToByteCodeType(returnType)
          , parameters.map(p => javaTypeToByteCodeType(JavaMethodModel.getParameterType(p))))
      }

    }
    def javaTypeToByteCodeType(_type: Any) : Any = {
      _type
    }


  }


  def getQualifiedClassName(clazz: MetaObject): QualifiedClassName = {
    val className = JavaClassModel.getClassName(clazz)
    val qualifiedClassName = new QualifiedClassName(JavaClassModel.getPackage(clazz) ++ Seq(className))
    qualifiedClassName
  }

  def dependencies: Set[ProgramTransformation] = Set(ByteCodeGoTo)
}
