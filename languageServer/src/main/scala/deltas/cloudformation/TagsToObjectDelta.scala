package deltas.cloudformation

import core.deltas.DeltaWithPhase
import core.deltas.path._
import core.language.Compilation
import core.language.node.Node
import deltas.json.{JsonObjectLiteralDelta, JsonStringLiteralDelta}
import deltas.yaml.YamlLanguageDelta

object RemoveTagsInObjectMemberKeys extends DeltaWithPhase {
  override def transformProgram(program: Node, compilation: Compilation): Unit = {

    PathRoot(program).visitShape(YamlLanguageDelta.TaggedNode, path => {
      val value = path.current(YamlLanguageDelta.TagNode)
      path match {
        case fp: FieldPath if fp.field == JsonObjectLiteralDelta.MemberKey => fp.replaceWith(value)
        case _ =>
      }
    })

    PathRoot(program).visitShape(JsonObjectLiteralDelta.MemberShape, path => {
      val key = path(JsonObjectLiteralDelta.MemberKey).asInstanceOf[NodeChildPath]
      key.current.shape match {
        case JsonStringLiteralDelta.Shape => key.replaceWith(key.current(JsonStringLiteralDelta.Value))
        //case YamlLanguageDelta.TaggedNode => key.replaceWith(key.current(YamlLanguageDelta.TagNode))
      }
    })

  }

  override def description = "RemoveTagsInObjectMemberKeys"

  override def dependencies = Set.empty
}

object TagsToObjectDelta extends DeltaWithPhase {
  import JsonObjectLiteralDelta._

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    PathRoot(program).visitShape(YamlLanguageDelta.TaggedNode, path => {
      val tagName: String = path.current(YamlLanguageDelta.TagName).asInstanceOf[String]
      val tagValue: Any = path.current(YamlLanguageDelta.TagNode)
      val newNode = Shape.create(Members -> Seq(
        MemberShape.create(
          MemberKey -> ("Fn::" + tagName),
          MemberValue -> tagValue)
      ))
      path.asInstanceOf[ChildPath].replaceWith(newNode)
    })
  }

  override def description = "Rewrite YAML tags into JSON objects"

  override def dependencies = Set(YamlLanguageDelta)
}
