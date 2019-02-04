package deltas.solidity

import core.deltas.Delta
import core.deltas.path.PathRoot
import core.language.Language
import core.smarts.types.objects.TypeFromDeclaration
import deltas.ConstraintSkeleton
import deltas.javac.types.BooleanTypeDelta

object SolidityLibraryDelta extends Delta {

  override def inject(language: Language): Unit = {
    language.collectConstraints = (compilation, builder) => {
      val rootScope = builder.newScope(debugName = "rootScope")

      val msgType = builder.declare("<MSGDECLARATION>", rootScope) // TODO get rid of fake declarations
      val msgScope = builder.declareScope(msgType, Some(rootScope), "msgScope")
      val message = builder.declare("msg", rootScope, _type = Some(TypeFromDeclaration(msgType)))

      builder.declare("data", msgScope)
      builder.declare("gas", msgScope)
      builder.declare("sender", msgScope)
      builder.declare("sig", msgScope)
      builder.declare("value", msgScope)

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
