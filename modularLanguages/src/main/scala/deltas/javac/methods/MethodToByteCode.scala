package deltas.javac.methods

import core.deltas.{Contract, Delta}
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node.Node
import deltas.bytecode.ByteCodeMethodInfo
import deltas.bytecode.attributes.CodeAttributeDelta.{CodeAttributesKey, CodeExceptionTableKey, CodeMaxLocalsKey, Instructions}
import deltas.bytecode.attributes.{AttributeNameKey, CodeAttributeDelta}
import deltas.bytecode.constants.Utf8ConstantDelta
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import deltas.javac.classes.ClassCompiler
import deltas.javac.classes.skeleton.JavaClassDelta
import deltas.javac.expressions.ToByteCodeSkeleton
import deltas.javac.methods.MethodDelta.{Body, Method, getMethodCompiler, getMethodDescriptor, setMethodCompiler}

object MethodToByteCode {

  def dependencies = Set[Contract](InferredMaxStack, InferredStackFrames)

  def compile(compilation: Compilation, method: NodePath): Node = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)

    convertMethod(method, classCompiler, compilation)
    method.asNode
  }

  def convertMethod(method: Method[NodePath], classCompiler: ClassCompiler, compilation: Compilation): Unit = {

    method.shape = ByteCodeMethodInfo.Shape
    AccessibilityFieldsDelta.addAccessFlags(method)
    method(ByteCodeMethodInfo.MethodNameIndex) = Utf8ConstantDelta.create(method.name)
    val methodDescriptorIndex = getMethodDescriptor(method.current, classCompiler)
    method(ByteCodeMethodInfo.MethodDescriptor) = methodDescriptorIndex
    addCodeAnnotation(method)

    //    method.current.data.remove(Name) // TODO bring these back.
    //    method.current.data.remove(ReturnType)
    //    method.current.data.remove(Parameters)

    def addCodeAnnotation(method: NodePath): Unit = {
      setMethodCompiler(method, compilation)
      val statementToInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
      val instructions = statementToInstructions(method.body)
      val exceptionTable = Seq[Node]()
      val codeAttributes = Seq[Node]()
      val methodCompiler = getMethodCompiler(compilation)
      val maxLocalCount: Int = methodCompiler.variablesPerStatement.values.map(pool => pool.localCount).max //TODO move this to a lower level.
      val codeAttribute = new Node(CodeAttributeDelta.CodeKey,
        AttributeNameKey -> CodeAttributeDelta.constantEntry,
        CodeMaxLocalsKey -> maxLocalCount,
        Instructions -> instructions,
        CodeExceptionTableKey -> exceptionTable,
        CodeAttributesKey -> codeAttributes)
      method(ByteCodeMethodInfo.MethodAttributes) = Seq(codeAttribute)
      method.current.data.remove(Body)
    }
  }
}
