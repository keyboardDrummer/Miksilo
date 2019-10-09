package deltas.cloudformation

import core.deltas.DeltaWithPhase
import core.deltas.path._
import core.language.Compilation
import core.language.node.{FieldData, Node}
import deltas.json.JsonObjectLiteralDelta
import deltas.yaml.YamlCoreDelta

object ConvertTagsToObjectDelta extends DeltaWithPhase {
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
      path.range.foreach(r => newNode.sources.put(Members, r)) // TODO it would be nice if we could leave this out, if members would inherit the source position from their chidlren.
      path.asInstanceOf[ChildPath].replaceWith(newNode)
    })
  }

  override def description = "Rewrite YAML tags into JSON objects"

  override def dependencies = Set(YamlCoreDelta)
}
