package miksilo.modularLanguages.deltas.javac.methods

import miksilo.modularLanguages.core.deltas.{Contract, Delta}
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.ByteCodeMethodInfo
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.{CodeAttributesKey, CodeExceptionTableKey, CodeMaxLocalsKey, Instructions}
import miksilo.modularLanguages.deltas.bytecode.attributes.{AttributeNameKey, CodeAttributeDelta}
import miksilo.modularLanguages.deltas.bytecode.constants.Utf8ConstantDelta
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import miksilo.modularLanguages.deltas.javac.classes.ClassCompiler
import miksilo.modularLanguages.deltas.javac.classes.skeleton.JavaClassDelta
import miksilo.modularLanguages.deltas.javac.expressions.ToByteCodeSkeleton
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.method.MethodDelta.{Body, Method}

object MethodToByteCode {

  def dependencies: Set[Contract] = Set[Contract](InferredMaxStack, InferredStackFrames)

  def compile(compilation: Compilation, method: NodePath): Node = {
    val classCompiler = JavaClassDelta.getClassCompiler(compilation)

    convertMethod(method, classCompiler, compilation)
    method.asNode
  }

  def convertMethod(method: Method[NodePath], classCompiler: ClassCompiler, compilation: Compilation): Unit = {

    method.shape = ByteCodeMethodInfo.Shape
    AccessibilityFieldsDelta.addAccessFlags(method)
    method(ByteCodeMethodInfo.MethodNameIndex) = Utf8ConstantDelta.create(method.name)
    val methodDescriptorIndex = MethodDelta.getMethodDescriptor(method.current, classCompiler)
    method(ByteCodeMethodInfo.MethodDescriptor) = methodDescriptorIndex
    addCodeAnnotation(method)

    //    method.current.data.remove(Name) // TODO bring these back.
    //    method.current.data.remove(ReturnType)
    //    method.current.data.remove(Parameters)

    def addCodeAnnotation(method: NodePath): Unit = {
      MethodDelta.setMethodCompiler(method, compilation)
      val statementToInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
      val instructions = statementToInstructions(method.body)
      val exceptionTable = Seq[Node]()
      val codeAttributes = Seq[Node]()
      val methodCompiler = MethodDelta.getMethodCompiler(compilation)
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
