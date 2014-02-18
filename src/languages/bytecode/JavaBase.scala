package languages.bytecode

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import scala.collection.mutable
import javaInterpreter.{JavaClass}

class VariablePool {
  var offset = 0
  val variables = mutable.Map[String,Integer]()

  def add(variable: String, _type: Any) {
    variables(variable) = offset
    offset += JavaBase.getSize(_type)
  }
}

case class MethodInfo(location: Integer, method: MetaObject)

class InstructionCompiler(val transformationState: TransformationState) {
  def findMethod(name: String, argumentTypes: Seq[MetaObject]) : MethodInfo = ???
  val variables = new VariablePool()
  var localCount = 0
}

object JavaBase extends ProgramTransformation {

  def clazz(methods: Seq[MetaObject]) = new MetaObject(classOf[JavaClass].getSimpleName) {
    data.put("methods", methods)
  }

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
      // val argumentTypes = callArguments.
      callCallee.clazz match {
        case VariableKey =>
          compiler.findMethod(getVariableName(callCallee), Seq())
        case SelectorKey =>
      }
      val functionName = getVariableName(???)
      ???
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
      val constantPool = new ConstantPool()
      val methods = getMethods(clazz)
      for(method <- methods)
        convertMethod(method)

      def addCodeAnnotation(method: MetaObject, parameters: Seq[MetaObject]) {
        val instructionCompiler = getInstructionCompiler(parameters)
        val statements = getMethodBody(method)
        val instructions = statements.flatMap(statement => statementToInstructions(statement, instructionCompiler))
        val codeIndex = constantPool.storeUtf8("Code")
        val exceptionTable = Seq[MetaObject]()
        val codeAttributes = Seq[MetaObject]()
        val length = ???
        ByteCode.codeAttribute(codeIndex, length, getMaxStack(instructions), instructionCompiler.localCount,
          instructions, exceptionTable, codeAttributes)
      }

      def getInstructionCompiler(parameters: Seq[MetaObject]) = {
        val methodState = new InstructionCompiler(state)
        for (parameter <- parameters)
          methodState.variables.add(getParameterName(parameter), getParameterType(parameter))
        methodState
      }

      def convertMethod(method: MetaObject) {
        val index = constantPool.storeUtf8(getMethodName(method))
        method(ByteCode.nameIndex) = index
        method.data.remove(methodNameKey)
        val parameters = getMethodParameters(method)
        addMethodDescriptor(method, parameters)
        addCodeAnnotation(method, parameters)

        method.data.remove(methodParametersKey)
      }

      def addMethodDescriptor(method: MetaObject, parameters: Seq[MetaObject]) {
        val returnType = getMethodReturnType(method)
        method.data.remove(ReturnTypeKey)

        ByteCode.methodDescriptor(javaTypeToByteCodeType(returnType)
            ,parameters.map(p => javaTypeToByteCodeType(getParameterType(p))))
      }

      def javaTypeToByteCodeType(_type: Any) : Any = {
        _type
      }

    }

  }

  def getMethodBody(metaObject: MetaObject) = metaObject(methodBodyKey).asInstanceOf[Seq[MetaObject]]
  def getMethods(clazz: MetaObject) = clazz("methods").asInstanceOf[Seq[MetaObject]]

  val methodBodyKey: String = "_body"

  val parameterNameKey: String = "name"

  object StaticKey
  object VisibilityKey
  val publicVisibility = "public"
  val privateVisibility = "private"
  def method(name: String, _returnType: Any, _parameters: Seq[MetaObject], _body: Seq[MetaObject],
               static: Boolean = false, visibility: String = privateVisibility) = {
    new MetaObject("JavaMethod") {
      data.put(parameterNameKey, name)
      data.put(ReturnTypeKey, _returnType)
      data.put(methodParametersKey, _parameters)
      data.put(methodBodyKey,_body)
      data.put(StaticKey, static)
      data.put(VisibilityKey, visibility)
    }
  }

  object VariableKey
  val variableNameKey = "name"
  def variable(name: String) = {
    new MetaObject(VariableKey) {
      data.put(variableNameKey, name)
    }
  }
  def getVariableName(variable: MetaObject) = variable(variableNameKey).asInstanceOf[String]

  object CallKey
  object CallCallee
  object CallArguments
  def call(callee: MetaObject, arguments: Seq[MetaObject] = Seq()) = {
    new MetaObject(CallKey) {
      data.put(CallCallee, callee)
      data.put(CallArguments, arguments)
    }
  }

  def getCallCallee(call: MetaObject) = call(CallCallee).asInstanceOf[MetaObject]
  def getCallArguments(call: MetaObject) = call(CallArguments).asInstanceOf[Seq[MetaObject]]

  def parameter(name: String, _type: Any) = {
    new MetaObject("JavaParameter") {
      data.put(parameterNameKey, name)
      data.put(parameterTypeKey, _type)
    }
  }

  def getParameterType(metaObject: MetaObject) : Any = metaObject(parameterTypeKey)
  def getParameterName(metaObject: MetaObject) = metaObject(parameterNameKey).asInstanceOf[String]
  val parameterTypeKey: String = "_type"

  val methodParametersKey: String = "parameters"
  def getMethodParameters(metaObject: MetaObject) = {
    metaObject(methodParametersKey).asInstanceOf[Seq[MetaObject]]
  }

  def getMethodReturnType(metaObject: MetaObject) = {
    metaObject(ReturnTypeKey)
  }

  object ReturnTypeKey
  val methodNameKey: String = parameterNameKey

  def getMethodName(metaObject: MetaObject) = {
    metaObject(methodNameKey).asInstanceOf[String]
  }

  def arrayType(elementType: Any) = {
    new MetaObject("arrayType") { data.put("elementType", elementType) }
  }

  object StringType
  object VoidType
  object IntegerType
  object DoubleType
  class ConstantPool {
    val constants: Seq[Any] = mutable.Seq()
    val reverseRouter = mutable.Map[Any,Integer]()

    def storeUtf8(value: String) = {
      val index = constants.length
      reverseRouter(value) = index
      constants +: value
      index
    }
  }

  object SelectorKey
  object SelectorObject
  object SelectorMember
  def selector(selectee: MetaObject, member: String) {
    new MetaObject(SelectorKey) {
      data.put(SelectorObject, selectee)
      data.put(SelectorMember, member)
    }
  }

  def dependencies: Set[ProgramTransformation] = Set(ByteCodeGoTo)
}
