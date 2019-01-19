package deltas.solidity

import core.deltas.Delta
import core.deltas.path.PathRoot
import core.language.Language
import deltas.ConstraintSkeleton
import deltas.javac.types.BooleanTypeDelta

object SolidityLibraryDelta extends Delta {

  override def inject(language: Language): Unit = {
    language.collectConstraints = (compilation, builder) => {
      val rootScope = builder.newScope(debugName = "rootScope")

      val assertType = SolidityFunctionTypeDelta.createType(compilation, builder, rootScope, Seq(BooleanTypeDelta.booleanType), Seq.empty)
      builder.declare("assert", rootScope, _type = Some(assertType))

      builder.declare("require", rootScope, _type = Some(assertType))

      val revertType = SolidityFunctionTypeDelta.createType(compilation, builder, rootScope, Seq.empty, Seq.empty)
      builder.declare("revert", rootScope, _type = Some(revertType))
      ConstraintSkeleton.constraints(compilation, builder, PathRoot(compilation.program), rootScope)
    }
  }

  override def description = "Adds the solidity standard library"

  override def dependencies = Set.empty
}
