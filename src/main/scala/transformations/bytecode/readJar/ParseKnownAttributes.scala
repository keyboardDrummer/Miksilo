package transformations.bytecode.readJar

import core.particles.node.Node
import core.particles.{CompilationState, DeltaWithPhase}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.AttributeNameKey
import transformations.bytecode.attributes.{ByteCodeAttribute, UnParsedAttribute}
import transformations.bytecode.attributes.UnParsedAttribute.UnParsedAttribute
import transformations.bytecode.ByteCodeSkeleton._

object ParseKnownAttributes extends DeltaWithPhase {
  override def transform(program: Node, state: CompilationState): Unit = {
    val constantPool = program.constantPool
    program.visit(node => node.clazz match {
      case UnParsedAttribute.UnParsedAttributeKey =>
        val typedNode = new UnParsedAttribute.UnParsedAttribute(node)
        val index = typedNode.nameIndex
        val name = constantPool.getValue(index).asInstanceOf[String]
        val attributeTypeOption = ByteCodeSkeleton.getState(state).attributes.get(name)
        for(attributeType <- attributeTypeOption)
        {
          parseAttribute(typedNode, attributeType)
        }
        true
      case _ => true
    })
  }

  def parseAttribute(typedNode: UnParsedAttribute, attributeType: ByteCodeAttribute): Unit = {
    val parseWithoutNameIndex: ClassFileParser.Parser[Node] = attributeType.getParser(typedNode.node)
    val parser = parseWithoutNameIndex.map(node => {
      node(AttributeNameKey) = typedNode.nameIndex
      node
    })
    val inputBytes = typedNode.data.toArray
    val parseResult = parser(new ArrayReader(0, inputBytes))
    val newNode = parseResult.get
    typedNode.node.replaceWith(newNode)
  }

  override def description: String = "In the initial parsing of bytecode, the attributes are not parsed. " +
    "This phase parses the attributes, but only if the attribute type is known by the compiler."
}
