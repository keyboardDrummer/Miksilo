package transformations.javac

import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.base.model.{JavaTypes, QualifiedClassName}
import transformations.javac.base.{MethodInfo, PackageInfo}

object JavaLang {
  val javaPackageName: String = "java"
  val langPackageName: String = "lang"
  val ioPackageName = "io"
  val standardLib = new PackageInfo(None, "")
  val javaPackage = standardLib.newPackageInfo(javaPackageName)
  val langPackage = javaPackage.newPackageInfo(langPackageName)
  val objectClass = langPackage.newClassInfo(ImplicitObjectSuperClass.objectName)
  objectClass.content(ConstructorC.constructorName) =
    new MethodInfo(ByteCodeSkeleton.methodDescriptor(JavaTypes.voidType, Seq()), false)

  val systemClass = langPackage.newClassInfo("System")
  systemClass.newFieldInfo("out", JavaTypes.objectType(new QualifiedClassName(Seq(javaPackageName, ioPackageName, "PrintStream"))))

  val javaIO = javaPackage.newPackageInfo(ioPackageName)
  val printStreamClass = javaIO.newClassInfo("PrintStream")
  printStreamClass.newMethodInfo("print", ByteCodeSkeleton.methodDescriptor(JavaTypes.voidType, Seq(JavaTypes.intType)), _static = false)

  val stringClass = langPackage.newClassInfo("String")
}
