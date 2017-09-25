package transformations.bytecode.readJar

import core.particles.node.Node
import core.particles.{Compilation, DeltaWithPhase, Language}
import transformations.bytecode.attributes.SignatureAttribute
import transformations.bytecode.constants.ClassInfoConstant
import transformations.bytecode.{ByteCodeFieldInfo, ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.javac.classes.skeleton.{JavaClassSkeleton, QualifiedClassName}
import transformations.javac.classes.{ConstantPool, FieldDeclaration}
import transformations.javac.methods.MethodC
import transformations.javac.methods.MethodC.{DefaultVisibility, Visibility}
import transformations.bytecode.types.TypeSkeleton
import transformations.javac.types.{MethodType, TypeAbstraction}

import scala.collection.mutable.ArrayBuffer

object DecompileByteCodeSignature extends DeltaWithPhase {
  override def transform(program: Node, state: Compilation): Unit = {
    val constantPool = program.constantPool
    val classReference = constantPool.getNode(program(ByteCodeSkeleton.ClassNameIndexKey).asInstanceOf[Int])
    val nameIndex = classReference(ClassInfoConstant.ClassRefName).asInstanceOf[Int]
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

  val accessFlagsToVisibility: Map[ByteCodeMethodInfo.MethodAccessFlag, Visibility] = MethodC.visibilityAccessFlagLinks.
    flatMap(p => p._2.map(flag => (flag, p._1))).toMap
  def getMethods(state: Language, constantPool: ConstantPool, methodInfos: Seq[Node]): Seq[Node] = {
    methodInfos.map(methodInfo => {
      val nameIndex: Int = methodInfo(ByteCodeMethodInfo.MethodNameIndex).asInstanceOf[Int]
      val attributes = methodInfo(ByteCodeMethodInfo.MethodAttributes).asInstanceOf[Seq[Node]]
      val signatureAttribute = attributes.find(node => node.clazz == SignatureAttribute.key)
      val _type: Node = signatureAttribute match {
        case Some(signature) =>
          val signatureIndex = signature(SignatureAttribute.SignatureIndex).asInstanceOf[Int]
          val signatureTypeString = constantPool.getUtf8(signatureIndex)
          TypeSkeleton.getTypeFromByteCodeString(state, signatureTypeString)
        case None =>
          val methodDescriptorIndex = methodInfo(ByteCodeMethodInfo.MethodDescriptorIndex).asInstanceOf[Int]
          val descriptor = constantPool.getUtf8(methodDescriptorIndex)
          val descriptorType: Node = TypeSkeleton.getTypeFromByteCodeString(state, descriptor)
          descriptorType
      }
      val name: String = constantPool.getUtf8(nameIndex)

      val (methodType, typeParameters) =
        if (_type.clazz == TypeAbstraction.TypeAbstractionKey)
          (TypeAbstraction.getBody(_type),TypeAbstraction.getParameters(_type))
        else
          (_type,Seq.empty)
      val returnType = methodType(MethodType.ReturnType).asInstanceOf[Node]
      val parameterTypes = methodType(MethodType.Parameters).asInstanceOf[Seq[Node]]
	    val parameters = parameterTypes.zipWithIndex.map(parameterTypeWithIndex =>
        MethodC.parameter("parameter" + parameterTypeWithIndex._2, parameterTypeWithIndex._1))

      val accessFlags: Set[ByteCodeMethodInfo.MethodAccessFlag] = Set.empty //TODO fix.
      val foundVisibilities: Set[Visibility] = accessFlags.flatMap(f => accessFlagsToVisibility.get(f))
      val visibility: Visibility = (foundVisibilities ++ Seq(DefaultVisibility)).head
      val static: Boolean = false //TODO fix.
      MethodC.method(name, returnType, parameters, Seq.empty, static, visibility, typeParameters)
    })
  }

  def getFields(state: Language, constantPool: ConstantPool, fieldInfos: Seq[Node]): Seq[Node] = {
    fieldInfos.map(fieldInfo => {
      val nameIndex: Int = fieldInfo(ByteCodeFieldInfo.NameIndex).asInstanceOf[Int]
      val attributes = fieldInfo(ByteCodeFieldInfo.FieldAttributes).asInstanceOf[Seq[Node]]
      val signatureAttribute = attributes.find(node => node.clazz == SignatureAttribute.key)
      val _type: Node = signatureAttribute match {
        case Some(signature) =>
          val signatureIndex = signature(SignatureAttribute.SignatureIndex).asInstanceOf[Int]
          val signatureTypeString = constantPool.getUtf8(signatureIndex)
          TypeSkeleton.getTypeFromByteCodeString(state, signatureTypeString)
        case None =>
          val fieldDescriptorIndex = fieldInfo(ByteCodeFieldInfo.DescriptorIndex).asInstanceOf[Int]
          val descriptor = constantPool.getUtf8(fieldDescriptorIndex)
          val descriptorType: Node = TypeSkeleton.getTypeFromByteCodeString(state, descriptor)
          descriptorType
      }
      val name: String = constantPool.getUtf8(nameIndex)
      FieldDeclaration.field(_type, name)      
    })
  }

  override def description: String = "Decompiles the field and method signatures in a classfile."
}
