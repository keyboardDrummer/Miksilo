package transformations.javac

import transformations.bytecode.ByteCode
import transformations.javac.base._
import scala.collection.mutable
import transformations.javac.base.PackageInfo
import transformations.javac.base.MethodInfo
import transformations.javac.base.model.{JavaTypes, QualifiedClassName}

object JavaLang {
  val javaPackageName: String = "java"
  val langPackageName: String = "lang"
  val ioPackageName = "io"
  val standardLib = new PackageInfo(None, "")
  val javaPackage = standardLib.newPackageInfo(javaPackageName)
  val langPackage = javaPackage.newPackageInfo(langPackageName)
  val objectClass = langPackage.newClassInfo(ImplicitObjectSuperClass.objectName)
  objectClass.content(ConstructorC.constructorName) =
      new MethodInfo(ByteCode.methodDescriptor(JavaTypes.VoidType,Seq()), false)

  val systemClass = langPackage.newClassInfo("System")
  systemClass.newFieldInfo("out", JavaTypes.objectType(new QualifiedClassName(Seq(javaPackageName,ioPackageName,"PrintStream"))))

  val javaIO = javaPackage.newPackageInfo(ioPackageName)
  val printStreamClass = javaIO.newClassInfo("PrintStream")
  printStreamClass.newMethodInfo("print",ByteCode.methodDescriptor(JavaTypes.VoidType, Seq(JavaTypes.IntType)), _static = false)

  val stringClass = langPackage.newClassInfo("String")
}
