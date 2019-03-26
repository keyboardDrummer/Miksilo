package deltas.cloudformation

import core.deltas.DeltaWithPhase
import core.deltas.path.{NodeChildPath, PathRoot}
import core.language.Compilation
import core.language.node.Node
import deltas.json.{JsonObjectLiteralDelta, StringLiteralDelta}
import deltas.yaml.YamlCoreDelta

object ConvertObjectMemberKeysToStrings extends DeltaWithPhase {
  override def transformProgram(program: Node, compilation: Compilation): Unit = {

    PathRoot(program).visitShape(JsonObjectLiteralDelta.MemberShape, path => {
      val key = path(JsonObjectLiteralDelta.MemberKey).asInstanceOf[NodeChildPath]
      key.current.shape match {
        case StringLiteralDelta.Shape => key.replaceWith(key.current(StringLiteralDelta.Value))
        case YamlCoreDelta.TaggedNode => key.replaceWith(key.current(YamlCoreDelta.TagNode).asInstanceOf[Node](StringLiteralDelta.Value))
        case _ => throw new Exception("Only string literals allowed")
      }
    })

  }

  override def description = "Remove tags in object members keys, and unbox string literal object member keys"

  override def dependencies = Set(JsonObjectLiteralDelta, YamlCoreDelta, StringLiteralDelta)
}
