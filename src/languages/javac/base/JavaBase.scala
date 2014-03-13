package languages.javac.base

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import scala.collection.mutable
import JavaBaseModel._
import languages.javac.base.JavaMethodModel._
import languages.bytecode.{NoStackFrame, NoMaxStack, ByteCodeGoTo, ByteCode}
import languages.javac.base.JavaTypes._
import languages.javac.base.MethodId
import languages.bytecode.ByteCode._
import languages.javac.base.MethodInfo
import scala.Some
import languages.javac.base.MethodId
import languages.javac.base.ClassOrObjectReference
import languages.javac.base.QualifiedClassName
import languages.javac.base.MethodCompiler
import languages.javac.base.MethodInfo
import scala.Some
import languages.javac.base.MethodId
import languages.javac.base.ClassOrObjectReference
import languages.javac.base.QualifiedClassName
import languages.javac.base.MethodCompiler


object JavaBase extends ProgramTransformation {


  def getTypeSize(_type: Any): Int = _type match {
    case IntegerType => 1
    case BooleanType => 1
    case DoubleType => 1
    case meta: MetaObject => meta.clazz match  {
      case JavaTypes.ArrayType => 1
      case ObjectType => 1
    }
    case VoidType => 0
  }

  def getStatementToLines(state: TransformationState) = state.data.getOrElseUpdate(this, getInitialStatementToLines)
    .asInstanceOf[mutable.Map[AnyRef, (MetaObject, MethodCompiler) => Seq[MetaObject]]]

  def getInitialStatementToLines = {
    val result = mutable.Map.empty[AnyRef, (MetaObject, MethodCompiler) => Seq[MetaObject]]
    result.put(VariableKey, (variable, compiler) => {
      val variableAddress = compiler.variables.variables(getVariableName(variable)).offset
      Seq(ByteCode.integerLoad(variableAddress))
    })

    result.put(CallKey, callToLines)
    result.put(Return, returnToLines)
    result.put(SelectorKey, selectorToLines)
    result
  }

  def selectorToLines(selector: MetaObject, compiler: MethodCompiler) : Seq[MetaObject] = {
    val obj = JavaBaseModel.getSelectorObject(selector)
    val member = JavaBaseModel.getSelectorMember(selector)
    val classOrObjectReference = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]
    val fieldInfo = classOrObjectReference.info.getField(member)
    val fieldRef = compiler.classCompiler.getFieldRefIndex(fieldInfo)
    if (classOrObjectReference.wasClass)
      Seq(ByteCode.getStatic(fieldRef))
    else
      ???
  }

  def returnToLines(_return: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val mbValue = getReturnValue(_return)
    mbValue match {
      case Some(value) =>
        val returnValueInstructions = statementToInstructions(value, compiler)
        returnValueInstructions ++ Seq(ByteCode.integerReturn)
      case None => Seq(ByteCode.voidReturn)
    }
  }

  def callToLines(call: MetaObject, compiler: MethodCompiler): Seq[MetaObject] = {
    val callCallee = getCallCallee(call)
    val obj = JavaBaseModel.getSelectorObject(callCallee)
    val member = JavaBaseModel.getSelectorMember(callCallee)
    val kind = compiler.getReferenceKind(obj).asInstanceOf[ClassOrObjectReference]

    val methodKey: MethodId = new MethodId(kind.info.getQualifiedName, member)
    val methodInfo = compiler.classCompiler.compiler.find(methodKey)
    val staticCall = methodInfo._static
    val calleeInstructions = if (!staticCall) statementToInstructions(obj, compiler) else Seq[MetaObject]()
    val callArguments = getCallArguments(call)
    val argumentInstructions = callArguments.flatMap(argument => statementToInstructions(argument, compiler))
    val methodRefIndex = compiler.classCompiler.getMethodRefIndex(methodKey)
    val invokeInstructions = Seq(if (staticCall)
      ByteCode.invokeStatic(methodRefIndex) else
      ByteCode.invokeVirtual(methodRefIndex))
    calleeInstructions ++ argumentInstructions ++ invokeInstructions
  }

  def statementToInstructions(statement: MetaObject, instructionCompiler: MethodCompiler)
    : Seq[MetaObject] = {
    val statementToSSMLines = getStatementToLines(instructionCompiler.transformationState)
    statementToSSMLines(statement.clazz)(statement, instructionCompiler)
  }

  def addMethodFlags(method: MetaObject) = {
    var flags = Set[ByteCode.MethodAccessFlag]()
    if (JavaMethodModel.getMethodStatic(method))
      flags += ByteCode.StaticAccess

    JavaMethodModel.getMethodVisibility(method) match
    {
      case JavaMethodModel.PublicVisibility => flags += ByteCode.PublicAccess
      case JavaMethodModel.PrivateVisibility => flags += ByteCode.PrivateAccess
    }

    method(ByteCode.MethodAccessFlags) = flags
  }

  def transform(program: MetaObject, state: TransformationState): Unit = {
    transformClass(program)

    def transformClass(clazz: MetaObject) {
      val classCompiler = new ClassCompiler(clazz, state)
      val classInfo = classCompiler.currentClassInfo
      clazz(ByteCode.ClassAttributes) = Seq()

      val classRef = classCompiler.getClassRef(classInfo)
      clazz(ByteCode.ClassNameIndexKey) = classRef
      val parentName = JavaClassModel.getParent(clazz).get
      val parentRef = classCompiler.getClassRef(classCompiler.fullyQualify(parentName))
      clazz(ByteCode.ClassParentIndex) = parentRef
      clazz(ByteCode.ClassInterfaces) = Seq()
      clazz(ByteCode.ClassFields) = Seq()
      clazz(ByteCode.ClassConstantPool) = classCompiler.constantPool.constants
      val methods = JavaClassModel.getMethods(clazz)
      for(method <- methods)
        bindMethod(method)
      
      for(method <- methods)
        convertMethod(method)

      def bindMethod(method: MetaObject) = {
        val methodName: String = JavaMethodModel.getMethodName(method)
        val descriptor = getMethodDescriptor(method)
        classInfo.content(methodName) = new MethodInfo(descriptor, JavaMethodModel.getMethodStatic(method))
      }

      def addCodeAnnotation(method: MetaObject) {
        val parameters = JavaMethodModel.getMethodParameters(method)
        val instructionCompiler = getMethodCompiler(method, parameters)
        val statements = JavaMethodModel.getMethodBody(method)
        val instructions = statements.flatMap(statement => statementToInstructions(statement, instructionCompiler))
        val codeIndex = classCompiler.constantPool.store(ByteCode.CodeAttributeId)
        val exceptionTable = Seq[MetaObject]()
        val codeAttributes = Seq[MetaObject]()
        val codeAttribute = new MetaObject(ByteCode.CodeKey) {
            data.put(AttributeNameKey, codeIndex)
            data.put(CodeMaxLocalsKey, instructionCompiler.localCount)
            data.put(CodeInstructionsKey, instructions)
            data.put(CodeExceptionTableKey, exceptionTable)
            data.put(CodeAttributesKey, codeAttributes)
          }
        method(ByteCode.MethodAnnotations) = Seq(codeAttribute)
      }

      def getMethodCompiler(method: MetaObject,parameters: Seq[MetaObject]) = {
        val methodCompiler = new MethodCompiler(classCompiler)
        if (!JavaMethodModel.getMethodStatic(method))
          methodCompiler.variables.add("this", JavaTypes.objectType(classCompiler.currentClassInfo.name))
        for (parameter <- parameters)
          methodCompiler.variables.add(JavaMethodModel.getParameterName(parameter), JavaMethodModel.getParameterType(parameter))
        methodCompiler
      }

      def convertMethod(method: MetaObject) {
        addMethodFlags(method)
        val methodNameIndex: Int = classCompiler.getMethodNameIndex(JavaMethodModel.getMethodName(method))
        method(ByteCode.MethodNameIndex) = methodNameIndex
        method.data.remove(JavaMethodModel.MethodNameKey)
        val methodDescriptorIndex = getMethodDescriptorIndex(method)
        method(ByteCode.MethodDescriptorIndex) = methodDescriptorIndex
        addCodeAnnotation(method)
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

    def javaTypeToByteCodeType(_type: Any) = _type match {
      case BooleanType => IntegerType
      case _ => _type
    }
  }


  def getQualifiedClassName(clazz: MetaObject): QualifiedClassName = {
    val className = JavaClassModel.getClassName(clazz)
    new QualifiedClassName(JavaClassModel.getPackage(clazz) ++ Seq(className))
  }

  def dependencies: Set[ProgramTransformation] = Set(NoMaxStack, NoStackFrame)
}
