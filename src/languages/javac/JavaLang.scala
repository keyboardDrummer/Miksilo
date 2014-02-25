package languages.javac

import languages.bytecode.ByteCode
import languages.javac.base.{MyPackage, ClassInfo, JavaTypes}
import languages.javac.base.{MethodInfo, MethodKey}
import scala.collection.mutable

object JavaLang {
  val javaPackageName: String = "java"
  val langPackageName: String = "lang"
  val javaLang = new MyPackage(mutable.Map(ImplicitObjectSuperClass.objectName ->
    new ClassInfo(mutable.Map(ConstructorC.constructorName ->
      new MethodInfo(ByteCode.methodDescriptor(JavaTypes.VoidType,Seq()))))))
  val standardLib : MyPackage = new MyPackage(mutable.Map(javaPackageName ->
    new MyPackage(mutable.Map(langPackageName -> javaLang))))
}
