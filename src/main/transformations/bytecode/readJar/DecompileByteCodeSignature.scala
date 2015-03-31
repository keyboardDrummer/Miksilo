package transformations.bytecode.readJar

import core.particles.node.Node
import core.particles.{CompilationState, ParticleWithPhase}
import transformations.bytecode.attributes.SignatureAttribute
import transformations.bytecode.constants.ClassRefConstant
import transformations.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.javac.classes.{ConstantPool, FieldDeclaration, JavaClassSkeleton, QualifiedClassName}
import transformations.javac.methods.MethodC
import transformations.types.{MethodTypeC, TypeSkeleton}

import scala.collection.mutable.ArrayBuffer

object DecompileByteCodeSignature extends ParticleWithPhase {
  override def transform(program: Node, state: CompilationState): Unit = {
    val constantPool = ByteCodeSkeleton.getConstantPool(program)
    val classReference = constantPool.getNode(program(ByteCodeSkeleton.ClassNameIndexKey).asInstanceOf[Int])
    val nameIndex = classReference(ClassRefConstant.ClassRefName).asInstanceOf[Int]
    val qualifiedClassName = new QualifiedClassName(constantPool.getUtf8(nameIndex).split("/").toSeq)

//    val parentClassReference = constantPool.getNode(program(ByteCodeSkeleton.ClassParentIndex).asInstanceOf[Int])
    //    val nameIndex = classReference(ClassRefConstant.ClassRefName).asInstanceOf[Int]
    //    val qualifiedClassName = new QualifiedClassName(constantPool.getUtf8(nameIndex).split("/").toSeq)

    val members = new ArrayBuffer[Node]()
    val javaClazz = JavaClassSkeleton.clazz(qualifiedClassName.parts.dropRight(1), qualifiedClassName.parts.last, members, List.empty[Node], None)
    
    val fieldInfos = program(ByteCodeSkeleton.ClassFields).asInstanceOf[Seq[Node]]

    members ++= getFields(state, constantPool, fieldInfos)
    members ++= getMethods(state, constantPool, program(ByteCodeSkeleton.ClassMethodsKey).asInstanceOf[Seq[Node]])

    program.replaceWith(javaClazz)
  }

  def getMethods(state: CompilationState, constantPool: ConstantPool, methodInfos: Seq[Node]): Seq[Node] = {
    methodInfos.map(methodInfo => {
      val nameIndex: Int = methodInfo(ByteCodeMethodInfo.MethodNameIndex).asInstanceOf[Int]
      val attributes = methodInfo(ByteCodeMethodInfo.MethodAttributes).asInstanceOf[Seq[Node]]
      val signatureAttribute = attributes.find(node => node.clazz == SignatureAttribute.key)
      val _type: Node = signatureAttribute match {
        case Some(signature) =>
          val signatureIndex = signature(SignatureAttribute.SignatureIndex).asInstanceOf[Int]
          val signatureTypeString = constantPool.getValue(signatureIndex).asInstanceOf[String]
          TypeSkeleton.getTypeFromByteCodeString(state, signatureTypeString)
        case None =>
          val fieldDescriptorIndex = methodInfo(ByteCodeMethodInfo.MethodDescriptorIndex).asInstanceOf[Int]
          val descriptor = constantPool.getValue(fieldDescriptorIndex).asInstanceOf[String]
          val descriptorType: Node = TypeSkeleton.getTypeFromByteCodeString(state, descriptor)
          descriptorType
      }
      val name: String = constantPool.getValue(nameIndex).asInstanceOf[String]
      val returnType = _type(MethodTypeC.ReturnType).asInstanceOf[Node]
      val parameterTypes = _type(MethodTypeC.Parameters).asInstanceOf[Seq[Node]]
	  val parameters = parameterTypes.zipWithIndex.map(parameterTypeWithIndex => MethodC.parameter("parameter" + parameterTypeWithIndex._2, parameterTypeWithIndex._1))
      MethodC.method(name, returnType, parameters, Seq.empty)
    })
  }
  
  def getFields(state: CompilationState, constantPool: ConstantPool, fieldInfos: Seq[Node]): Seq[Node] = {
    fieldInfos.map(fieldInfo => {
      val nameIndex: Int = fieldInfo(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]
      val attributes = fieldInfo(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[Node]]
      val signatureAttribute = attributes.find(node => node.clazz == SignatureAttribute.key)
      val _type: Node = signatureAttribute match {
        case Some(signature) =>
          val signatureIndex = signature(SignatureAttribute.SignatureIndex).asInstanceOf[Int]
          val signatureTypeString = constantPool.getValue(signatureIndex).asInstanceOf[String]
          TypeSkeleton.getTypeFromByteCodeString(state, signatureTypeString)
        case None =>
          val fieldDescriptorIndex = fieldInfo(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]
          val descriptor = constantPool.getValue(fieldDescriptorIndex).asInstanceOf[String]
          val descriptorType: Node = TypeSkeleton.getTypeFromByteCodeString(state, descriptor)
          descriptorType
      }
      val name: String = constantPool.getValue(nameIndex).asInstanceOf[String]
      FieldDeclaration.field(_type, name)      
    })
  }

  override def description: String = "Decompiles the field and method signatures in a classfile."
}
