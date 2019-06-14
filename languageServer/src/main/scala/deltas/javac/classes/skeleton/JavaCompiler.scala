package deltas.javac.classes.skeleton

import deltas.javac.JavaStandardLibrary
import deltas.javac.classes.{MethodInfo, MethodQuery}

class JavaCompiler {
  val classPath: PackageSignature = JavaStandardLibrary.classPath

  def getPackage(parts: List[String]): PackageSignature = classPath.getPackage(parts)

  def find(methodId: MethodQuery): MethodInfo = find(methodId.className.parts)
    .asInstanceOf[ClassSignature].getMethod(methodId)

  def find(parts: Seq[String]): PackageMember = parts.foldLeft[PackageMember](classPath)(
    (pck: PackageMember, part: String) => pck.asInstanceOf[PackageSignature].content(part))
}