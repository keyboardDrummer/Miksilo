package transformations.javac.classes.skeleton

import transformations.javac.JavaLang
import transformations.javac.classes.{MethodInfo, MethodQuery}

class JavaCompiler {
  val classPath: PackageSignature = JavaLang.classPath

  def getPackage(parts: List[String]): PackageSignature = classPath.getPackage(parts)

  def find(methodId: MethodQuery): MethodInfo = find(methodId.className.parts)
    .asInstanceOf[ClassSignature].getMethod(methodId)

  def find(parts: Seq[String]): PackageMember = parts.foldLeft[PackageMember](classPath)(
    (pck: PackageMember, part: String) => pck.asInstanceOf[PackageSignature].content(part))
}