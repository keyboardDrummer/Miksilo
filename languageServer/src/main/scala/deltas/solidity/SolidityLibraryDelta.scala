package deltas.solidity

import core.deltas.Delta
import core.deltas.path.PathRoot
import core.language.Language
import core.smarts.types.objects.TypeFromDeclaration
import deltas.ConstraintSkeleton
import deltas.bytecode.types.TypeSkeleton
import deltas.javac.types.BooleanTypeDelta

object SolidityLibraryDelta extends Delta {

  override def inject(language: Language): Unit = {
    language.collectConstraints = (compilation, builder) => {
      val rootScope = builder.newScope(debugName = "rootScope")

      val stringNode = ElementaryTypeDelta.neww("string")
      val bytesNode = ElementaryTypeDelta.neww("bytes")
      val stringToBytesConversionType = SolidityFunctionTypeDelta.createType(compilation, builder, rootScope, Seq(stringNode), Seq(bytesNode))
      builder.declare("bytes", rootScope, _type = Some(stringToBytesConversionType))

      val addressDeclaration = builder.resolveOption("address",None, rootScope, _type = Some(ElementaryTypeDelta.elementaryTypeConstructor))
      val addressScope = builder.getDeclaredScope(addressDeclaration)

      val uint256Node = ElementaryTypeDelta.neww("uint256")
      val uint256 = TypeSkeleton.getType(compilation, builder, uint256Node, rootScope)

      builder.declare("balance", addressScope, null, Some(uint256))
      val transferType = SolidityFunctionTypeDelta.createType(compilation, builder, rootScope, Seq(uint256Node), Seq.empty)
      builder.declare("transfer", addressScope, null, Some(transferType))

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

      builder.declare("now", rootScope, null, Some(uint256))
      ConstraintSkeleton.constraints(compilation, builder, PathRoot(compilation.program), rootScope)
    }
  }

  override def description = "Adds the solidity standard library"

  override def dependencies = Set.empty
}
