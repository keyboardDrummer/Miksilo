package transformations.javac

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.base.{ClassC, MethodC}

object ImplicitSuperConstructorCall extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(ConstructorC)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {

    for (constructor <- ClassC.getMethods(clazz).filter(method => method.clazz == ConstructorC.Constructor)) {
      val statements = MethodC.getMethodBody(constructor)
      var addSuperCall = false
      if (statements.isEmpty)
        addSuperCall = true
      else {
        val firstStatement = statements(0)
        if (firstStatement.clazz != ConstructorC.SuperCall && firstStatement.clazz != ConstructorC.ThisCall) {
          addSuperCall = true
        }
      }

      if (addSuperCall)
        constructor(MethodC.MethodBodyKey) = Seq(ConstructorC.superCall()) ++ statements
    }
  }
}
