package transformations.javac.classes.skeleton

import com.sun.tools.javac.resources.compiler
import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton.ByteCode
import transformations.javac.JavaLang
import transformations.javac.classes.{ClassCompiler, ConstantPool, MethodInfo, MethodQuery}
import transformations.javac.classes.skeleton.JavaClassSkeleton._

case class MyCompiler(state: CompilationState) {
  val classPath: PackageSignature = JavaLang.classPath

  def getPackage(parts: List[String]): PackageSignature = classPath.getPackage(parts)

  def find(methodId: MethodQuery): MethodInfo = find(methodId.className.parts)
    .asInstanceOf[ClassSignature].getMethod(methodId)

  def find(parts: Seq[String]): PackageMember = parts.foldLeft[PackageMember](classPath)(
    (pck: PackageMember, part: String) => pck.asInstanceOf[PackageSignature].content(part))
}