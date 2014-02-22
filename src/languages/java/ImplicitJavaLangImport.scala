package languages.java

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.java.base.JavaBase

object ImplicitJavaLangImport extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = ???
}
