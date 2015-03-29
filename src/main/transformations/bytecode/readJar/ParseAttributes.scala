package transformations.bytecode.readJar

import core.particles.{CompilationState, ParticleWithPhase}
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.attributes.UnParsedAttribute

import scala.util.parsing.input.{Position, Reader}



object ParseAttributes extends ParticleWithPhase {
  override def transform(program: Node, state: CompilationState): Unit = {
    val constantPool = ByteCodeSkeleton.getConstantPool(program)
    program.transform(node => node.clazz match {
      case UnParsedAttribute.UnParsedAttributeKey =>
        val index = node(UnParsedAttribute.UnParsedAttributeName).asInstanceOf[Int]
        val name = constantPool.getValue(index).asInstanceOf[String]
        val parser = ByteCodeSkeleton.getState(state).attributes(name).getParser(node)
        val inputBytes = node(UnParsedAttribute.UnParsedAttributeData).asInstanceOf[Seq[Byte]].toArray
        val parseResult = parser(new ArrayReader(0, inputBytes))
        val newNode = parseResult.get
        node.replaceWith(newNode)
      case _ =>
    })
  }

  override def description: String = "Parses attributes that still require it."
}
