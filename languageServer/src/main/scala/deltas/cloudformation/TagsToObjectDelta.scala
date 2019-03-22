package deltas.cloudformation

import core.deltas.DeltaWithPhase
import core.deltas.path._
import core.language.Compilation
import core.language.node.{FieldData, Node}
import deltas.json.{JsonObjectLiteralDelta, DoubleQuoteStringLiteralDelta}
import deltas.yaml.YamlCoreDelta

object RemoveTagsInObjectMemberKeys extends DeltaWithPhase {
  override def transformProgram(program: Node, compilation: Compilation): Unit = {

    PathRoot(program).visitShape(YamlCoreDelta.TaggedNode, path => {
      val value = path.current(YamlCoreDelta.TagNode)
      path match {
        case fp: FieldPath if fp.field == JsonObjectLiteralDelta.MemberKey => fp.replaceWith(value)
        case _ =>
      }
    })

    PathRoot(program).visitShape(JsonObjectLiteralDelta.MemberShape, path => {
      val key = path(JsonObjectLiteralDelta.MemberKey).asInstanceOf[NodeChildPath]
      key.current.shape match {
        case DoubleQuoteStringLiteralDelta.Shape => key.replaceWith(key.current(DoubleQuoteStringLiteralDelta.Value))
      }
    })

  }

  override def description = "RemoveTagsInObjectMemberKeys"

  override def dependencies = Set.empty
}

object TagsToObjectDelta extends DeltaWithPhase {
  import JsonObjectLiteralDelta._

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(YamlCoreDelta.TaggedNode, path => {
      val tagName: String = path.current(YamlCoreDelta.TagName).asInstanceOf[String]
      val tagValue: FieldData = path.getFieldData(YamlCoreDelta.TagNode)
      val newNode = Shape.create(Members -> Seq(
        MemberShape.createWithData(
          MemberKey -> ((if (tagName == "Ref" ) "" else "Fn::") + tagName),
          MemberValue -> tagValue)
      ))
      path.range.foreach(r => newNode.sources.put(Members, r)) // TODO it would be nice if we could leave this out, if members would inherit the source position from its chidlren.
      path.asInstanceOf[ChildPath].replaceWith(newNode)
    })
  }

  override def description = "Rewrite YAML tags into JSON objects"

  override def dependencies = Set(YamlCoreDelta)
}
