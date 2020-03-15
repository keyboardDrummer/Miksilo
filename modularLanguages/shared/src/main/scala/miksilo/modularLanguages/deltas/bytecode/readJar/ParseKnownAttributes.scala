package miksilo.modularLanguages.deltas.bytecode.readJar

import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithPhase}
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton._
import miksilo.modularLanguages.deltas.bytecode.attributes.UnParsedAttribute.UnParsedAttribute
import miksilo.modularLanguages.deltas.bytecode.attributes.{AttributeNameKey, ByteCodeAttribute, UnParsedAttribute}
import miksilo.modularLanguages.deltas.bytecode.constants.Utf8ConstantDelta

object ParseKnownAttributes extends DeltaWithPhase {

  override def description: String = "In the initial parsing of bytecode, the attributes are not parsed. " +
    "This phase parses the attributes, but only if the attribute type is known by the compiler."

  override def dependencies: Set[Contract] = Set[Contract](UnParsedAttribute, Utf8ConstantDelta)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val constantPool = program.constantPool
    program.visit(node => node.shape match {
          case UnParsedAttribute.Shape =>
            val typedNode = new UnParsedAttribute.UnParsedAttribute(node)
            val index = typedNode.nameIndex
            val name = constantPool.getValue(index).asInstanceOf[Node]
            val attributeTypeOption = ByteCodeSkeleton.attributesByName.get(compilation).get(Utf8ConstantDelta.get(name))
            for(attributeType <- attributeTypeOption)
            {
              parseAttribute(typedNode, attributeType)
            }
          case _ =>
        })
  }

  def parseAttribute(typedNode: UnParsedAttribute, attributeType: ByteCodeAttribute): Unit = {
    val parseWithoutNameIndex: ClassFileParser.Parser[Node] = attributeType.getParser(typedNode.node)
    val parser = parseWithoutNameIndex.map(node => {
      node(AttributeNameKey) = typedNode.nameIndex
      node
    })
    val inputBytes = typedNode.data.toArray
    val parseResult = ClassFileParser.phrase(parser)(new ByteReader(inputBytes))
    val newNode = parseResult.get
    typedNode.node.replaceData(newNode)
  }
}
