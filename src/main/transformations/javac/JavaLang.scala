package transformations.javac

import core.particles.node.Node
import transformations.bytecode.constants.MethodDescriptorConstant
import transformations.javac.classes.skeleton.{QualifiedClassName, JavaClassSkeleton, PackageInfo}
import transformations.javac.classes.MethodInfo
import transformations.javac.constructor.SuperCallExpression
import transformations.bytecode.types.{IntTypeC, ObjectTypeC, VoidTypeC}
import JavaClassSkeleton._
object JavaLang {

  def findPackageInfo(clazz: Node): PackageInfo = {
    clazz._package.foldLeft(classPath)((currentPackage, packagePart) => currentPackage.findOrCreate(packagePart))
  }

  def addToClassPath(clazz: Node): Unit = {
    val info = findPackageInfo(clazz)
    info.addClass(clazz)
  }

  val javaPackageName: String = "java"
  val langPackageName: String = "lang"
  val ioPackageName = "io"
  val classPath = new PackageInfo(None, "")
  val javaPackage = classPath.newPackageInfo(javaPackageName)
  val langPackage = javaPackage.newPackageInfo(langPackageName)
  val objectClass = langPackage.newClassInfo(ImplicitObjectSuperClass.objectName)
  objectClass.content(SuperCallExpression.constructorName) =
    new MethodInfo(MethodDescriptorConstant.methodDescriptor(VoidTypeC.voidType, Seq()), false)

  val systemClass = langPackage.newClassInfo("System")
  systemClass.newFieldInfo("out", ObjectTypeC.objectType(new QualifiedClassName(Seq(javaPackageName, ioPackageName, "PrintStream"))))

  val javaIO = javaPackage.newPackageInfo(ioPackageName)
  val printStreamClass = javaIO.newClassInfo("PrintStream")
  printStreamClass.newMethodInfo("print", MethodDescriptorConstant.methodDescriptor(VoidTypeC.voidType, Seq(IntTypeC.intType)), _static = false)
  printStreamClass.newMethodInfo("println", MethodDescriptorConstant.methodDescriptor(VoidTypeC.voidType, Seq(IntTypeC.intType)), _static = false)

  val stringClass = langPackage.newClassInfo("String")
}
