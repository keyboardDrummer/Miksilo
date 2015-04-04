package transformations.javac.classes.skeleton

import transformations.javac.JavaLang
import transformations.javac.classes.{MethodId, MethodInfo}

class MyCompiler {
  val classPath: PackageInfo = JavaLang.classPath

  def getPackage(parts: List[String]): PackageInfo = classPath.getPackage(parts)

  def find(methodId: MethodId): MethodInfo = find(methodId.className.parts)
    .asInstanceOf[ClassInfo].getMethod(methodId.methodName)

  def find(parts: Seq[String]): PackageMember = parts.foldLeft[PackageMember](classPath)(
    (pck: PackageMember, part: String) => pck.asInstanceOf[PackageInfo].content(part))
}