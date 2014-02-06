/*
 * Copyright (c) 2013 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.builder

import scala.collection.mutable
import org.modelexecution.fumldebug.core.util.ActivityFactory
import fUML.Syntax.Classes.Kernel._
import org.modelexecution.fumldebug.core.ExecutionContext
import fUML.Syntax.Classes.Kernel.Association
import fUML.Syntax.Activities.IntermediateActivities.Activity
import fUML.Syntax.Activities.IntermediateActivities.ActivityNode
import fUML.Syntax.Activities.IntermediateActivities.InitialNode
import fUML.Syntax.Activities.IntermediateActivities.ControlFlow
import fUML.Syntax.Activities.IntermediateActivities.FinalNode
import fUML.Syntax.Activities.IntermediateActivities.ActivityFinalNode
import fUML.Syntax.Classes.Kernel.Generalization
import fUML.Syntax.Classes.Kernel.Enumeration
import fUML.Semantics.Classes.Kernel.Object_
import fUML.Semantics.Classes.Kernel.FeatureValue
import fUML.Semantics.Classes.Kernel.Value
import fUML.Semantics.Classes.Kernel.StringValue
import fUML.Semantics.Classes.Kernel.BooleanValue
import fUML.Semantics.Classes.Kernel.IntegerValue
import fUML.Semantics.Classes.Kernel.EnumerationValue
import fUML.Semantics.Classes.Kernel.Link
import fUML.Syntax.Activities.IntermediateActivities.ActivityParameterNode
import fUML.Syntax.Actions.IntermediateActions.ReadStructuralFeatureAction
import fUML.Syntax.Actions.BasicActions.InputPin
import fUML.Syntax.Actions.BasicActions.OutputPin
import fUML.Syntax.Activities.IntermediateActivities.ObjectFlow
import fUML.Syntax.Activities.IntermediateActivities.ActivityEdge
import fUML.Syntax.Actions.BasicActions.InputPinList
import fUML.Syntax.Actions.BasicActions.Action
import fUML.Semantics.Classes.Kernel.Reference

trait FumlModelBuilder extends FumlModel {

  implicit def BuilderWrapper2Package(builder: BuilderWrapper[Package]) = builder.wrapped
  implicit def BuilderWrapper2Class_(builder: BuilderWrapper[Class_]) = builder.wrapped
  implicit def BuilderWrapper2Enumeration(builder: BuilderWrapper[Enumeration]) = builder.wrapped
  implicit def BuilderWrapper2Association(builder: BuilderWrapper[Association]) = builder.wrapped
  implicit def BuilderWrapper2Object_(builder: BuilderWrapper[Object_]) = builder.wrapped
  implicit def BuilderWrapper2Link(builder: BuilderWrapper[Link]) = builder.wrapped
  implicit def BuilderWrapper2Activity(builder: BuilderWrapper[Activity]) = builder.wrapped

  val STRING = ExecutionContext.getInstance().getPrimitiveStringType()
  val INTEGER = ExecutionContext.getInstance().getPrimitivIntegerType()
  val BOOLEAN = ExecutionContext.getInstance().getPrimitiveBooleanType()

  protected def packag(name: String) = {
    val packag = packages.get(name).getOrElse(createPackage(name))
    register[PackageBuilder](PackageBuilder(packag))
  }

  private def createPackage(name: String) = {
    val packag = new Package
    packag.name = name
    packag
  }

  protected def clazz(name: String) = {
    val clazz = classes.get(name).getOrElse(ActivityFactory.createClass(name))
    register[ClassBuilder](ClassBuilder(clazz))
  }

  protected def enum(name: String) = {
    val enum = enumerations.get(name).getOrElse(createEnumeration(name))
    register[EnumerationBuilder](EnumerationBuilder(enum))
  }

  private def createEnumeration(name: String) = {
    val enum = new Enumeration
    enum.name = name
    enum
  }

  protected def literal(name: String) = {
    val literal = new EnumerationLiteral
    literal.name = name
    literal
  }

  protected def attribute(name: String, propertyType: Type,
    lower: Int = 0, upper: Int = 1, isUnique: Boolean = false) = {
    property(name, propertyType, lower, upper, isUnique)
  }

  protected def property(name: String, propertyType: Type,
    lower: Int = 0, upper: Int = 1, isUnique: Boolean = false) = {
    val property = new Property();
    property.setName(name)
    property.setType(propertyType)
    property.setLower(lower)
    property.setUpper(upper)
    property.setIsUnique(isUnique)
    property
  }

  protected def association(name: String) = {
    val association = associations.get(name).getOrElse(createAssociation(name))
    register[AssociationBuilder](AssociationBuilder(association))
  }

  private def createAssociation(name: String) = {
    val association = new Association
    association.name = name
    association
  }

  def objectValue(id: String, clazz: Class_) = {
    val obj = objects.get(id).getOrElse(createObject(clazz))
    register[ObjectBuilder](ObjectBuilder(obj), id)
  }

  def createObject(clazz: Class_) = {
    val obj = new Object_()
    obj.types.add(clazz)
    obj
  }

  def link(id: String, association: Association) = {
    val link = links.get(id).getOrElse(createLink(association))
    register[LinkBuilder](LinkBuilder(link), id)
  }

  def createLink(association: Association) = {
    val obj = new Link
    obj.`type` = association
    obj
  }

  def featureValue(feature: StructuralFeature, value: String): FeatureValue = {
    val fValue = new StringValue
    fValue.value = value
    featureValue(feature, Set(fValue))
  }

  def featureValue(feature: StructuralFeature, value: Boolean): FeatureValue = {
    val fValue = new BooleanValue
    fValue.value = value
    featureValue(feature, Set(fValue))
  }

  def featureValue(feature: StructuralFeature, value: Integer): FeatureValue = {
    val fValue = new IntegerValue
    fValue.value = value
    featureValue(feature, Set(fValue))
  }

  def featureValue(feature: StructuralFeature, value: EnumerationLiteral): FeatureValue = {
    val fValue = new EnumerationValue
    fValue.literal = value
    fValue.`type` = value.enumeration
    featureValue(feature, Set(fValue))
  }

  def featureValue(feature: StructuralFeature, value: Object_): FeatureValue = {
    featureValue(feature, Set(toReference(value)))
  }
  
  private def toReference(objectValue : Object_) = {
    val reference = new Reference
    reference.referent = objectValue
    reference
  }

  def featureValue(feature: StructuralFeature, values: Set[_ <: Value]): FeatureValue = {
    val featureValue = new FeatureValue
    featureValue.feature = feature
    values.foreach(featureValue.values.add(_))
    featureValue
  }

  def activity(name: String) = {
    val activity = activities.get(name).getOrElse(ActivityFactory.createActivity(name))
    register[ActivityBuilder](ActivityBuilder(activity))
  }

  def parameterNode(name: String) = {
    val node = new ActivityParameterNode
    node.name = name
    node
  }

  def initialNode(name: String) = {
    val node = new InitialNode
    node.name = name
    node
  }

  def readFeatureNode(name: String, feature: StructuralFeature) = {
    val node = new ReadStructuralFeatureAction
    node.name = name
    
    val outputpin = new OutputPin()
    outputpin.setName("OutputPin result (" + name + ")")
    node.result = outputpin
    node.output.add(outputpin)

    val input_object = new InputPin()
    input_object.setName("InputPin object (" + name + ")")
    input_object.setLower(1)
    input_object.setUpper(1)
    node.`object` = input_object
    node.input.add(input_object)

    node.structuralFeature = feature
    node
  }

  def activityFinalNode(name: String) = {
    val node = new ActivityFinalNode
    node.name = name
    node
  }

}

trait FumlModel {

  val packages: mutable.Map[String, Package] = mutable.Map()
  val classes: mutable.Map[String, Class_] = mutable.Map()
  val enumerations: mutable.Map[String, Enumeration] = mutable.Map()
  val associations: mutable.Map[String, Association] = mutable.Map()
  val objects: mutable.Map[String, Object_] = mutable.Map()
  val links: mutable.Map[String, Link] = mutable.Map()
  val activities: mutable.Map[String, Activity] = mutable.Map()

  def register[T](element: T): T = {
    element match {
      case c: PackageBuilder => packages(c.packag.name) = c.packag
      case c: ClassBuilder => classes(c.clazz.name) = c.clazz
      case c: EnumerationBuilder => enumerations(c.enum.name) = c.enum
      case a: AssociationBuilder => associations(a.association.name) = a.association
      case a: ActivityBuilder => activities(a.activity.name) = a.activity
    }
    element
  }

  def register[T](element: T, id: String): T = {
    element match {
      case o: ObjectBuilder => objects(id) = o.obj
      case o: LinkBuilder => links(id) = o.link
    }
    element
  }

  def getPackage(name: String): Package = {
    packages.get(name).get
  }

  def getClass(name: String): Class_ = {
    classes.get(name).get
  }

  def getEnumeration(name: String): Enumeration = {
    enumerations.get(name).get
  }

  def getAssociation(name: String): Association = {
    associations.get(name).get
  }

  def getObject(id: String): Object_ = {
    objects.get(id).get
  }

  def getLink(id: String): Link = {
    links.get(id).get
  }

}

sealed abstract class BuilderWrapper[T](val wrapped: T) {

  def isClassifierWithName(obj: Object, name: String) = {
    obj match {
      case classifier: Classifier => name == classifier.name
      case _ => false
    }
  }

  def isLiteralWithName(obj: Object, name: String) = {
    obj match {
      case literal: EnumerationLiteral => name == literal.name
      case _ => false
    }
  }

  def isPropertyWithName(obj: Object, name: String) = {
    obj match {
      case property: Property => name == property.name
      case _ => false
    }
  }

  def isNodeWithName(obj: Object, name: String) = {
    obj match {
      case node: ActivityNode => name == node.name
      case _ => false
    }
  }

  def createMultiplicityElement(lower: Int, upper: Int) = {
    val me = new MultiplicityElement
    me.setLower(lower)
    me.setUpper(upper)
    me
  }

}

case class PackageBuilder(packag: Package) extends BuilderWrapper[Package](packag) {

  def withClassifiers(classifiers: Classifier*): PackageBuilder = {
    withClassifiers(classifiers.toSet[Classifier])
  }

  def withClassifiers(classifiers: Set[Classifier]): PackageBuilder = {
    classifiers.foreach(packag.ownedType.add(_))
    this
  }

  def classifier(name: String) = {
    packag.ownedType.toArray().find(isClassifierWithName(_, name))
  }

}

case class ClassBuilder(clazz: Class_) extends BuilderWrapper[Class_](clazz) {
  import scala.collection.JavaConversions._

  def extending(classes: `Class_`*): ClassBuilder = {
    extending(classes.toSet[Class_])
  }

  def extending(classes: Set[Class_]): ClassBuilder = {
    classes map (createGeneralization(_)) foreach (clazz.addGeneralization(_))
    this
  }

  private def createGeneralization(superClass: Class_) = {
    val generalziation = new Generalization();
    generalziation.setGeneral(superClass);
    generalziation.specific = clazz;
    generalziation
  }

  def withAttributes(properties: Property*): ClassBuilder = {
    withAttributes(properties.toSet[Property])
  }

  def withAttributes(properties: Set[Property]): ClassBuilder = {
    properties.foreach(clazz.addOwnedAttribute(_))
    this
  }

  def attribute(name: String) = {
    clazz.ownedAttribute.find(isPropertyWithName(_, name)).get
  }

}

case class EnumerationBuilder(enum: Enumeration) extends BuilderWrapper[Enumeration](enum) {
  import scala.collection.JavaConversions._

  def withLiterals(literals: EnumerationLiteral*): EnumerationBuilder = {
    withLiterals(literals.toSet[EnumerationLiteral])
  }

  def withLiterals(literals: Set[EnumerationLiteral]): EnumerationBuilder = {
    literals.foreach(enum.addOwnedLiteral(_))
    this
  }

  def literal(name: String) = {
    enum.ownedLiteral.find(isLiteralWithName(_, name)).get
  }

}

case class AssociationBuilder(association: Association) extends BuilderWrapper[Association](association) {
  import scala.collection.JavaConversions._

  def withProperties(properties: Property*): AssociationBuilder = {
    withProperties(properties.toSet[Property])
  }

  def withProperties(properties: Set[Property]): AssociationBuilder = {
    properties.foreach{property =>
      property.association = association
      association.memberEnd.add(property)
    }
    this
  }

  def property(name: String) = {
    association.memberEnd.find(isPropertyWithName(_, name)).get
  }

}

case class ObjectBuilder(obj: Object_) extends BuilderWrapper[Object_](obj) {

  def withAttributeValues(values: FeatureValue*): ObjectBuilder = {
    withAttributeValues(values.toSet[FeatureValue])
  }

  def withAttributeValues(values: Set[FeatureValue]): ObjectBuilder = {
    values.foreach(obj.featureValues.addValue(_))
    this
  }

}

case class LinkBuilder(link: Link) extends BuilderWrapper[Link](link) {

  def withValues(values: FeatureValue*): LinkBuilder = {
    withValues(values.toSet[FeatureValue])
  }

  def withValues(values: Set[FeatureValue]): LinkBuilder = {
    values.foreach(link.featureValues.addValue(_))
    this
  }

}

case class ActivityBuilder(activity: Activity) extends BuilderWrapper[Activity](activity) {

  def withNodes(nodes: ActivityNode*): ActivityBuilder = {
    withNodes(nodes.toSet[ActivityNode])
  }

  def withNodes(nodes: Set[ActivityNode]): ActivityBuilder = {
    nodes.foreach(activity.addNode(_))
    this
  }

  def withInput(node: ActivityParameterNode,
    paraType: Type = null, lower: Int = 1, upper: Int = 1): ActivityBuilder = {
    val parameter = createParameter(node, paraType, lower, upper)
    parameter.direction = ParameterDirectionKind.in
    this
  }

  def withOutput(node: ActivityParameterNode,
    paraType: Type = null, lower: Int = 1, upper: Int = 1): ActivityBuilder = {
    val parameter = createParameter(node, paraType, lower, upper)
    parameter.direction = ParameterDirectionKind.out
    this
  }

  private def createParameter(node: ActivityParameterNode,
    paraType: Type, lower: Int, upper: Int) = {
    val parameter = new Parameter
    parameter.multiplicityElement = createMultiplicityElement(lower, upper)
    parameter.name = node.name
    parameter.`type` = paraType
    node.parameter = parameter
    activity.addNode(node)
    activity.addOwnedParameter(parameter)
    parameter
  }

  def node(name: String) = {
    ConnectableNode(activity.node.toArray().find(isNodeWithName(_, name)))
  }
  
  def action(name: String) = {
    node(name).wrapped match {
      case Some(node) if node.isInstanceOf[Action] => node.asInstanceOf[Action]
      case _ => null
    }
  }

}

trait Connectable {
  
  def wrapped: Option[Object]

  val node = wrapped match {
    case w: Some[Object] => w.get match {
      case n: ActivityNode => n
      case _ => null
    }
    case _ => null
  }

  def +->(otherNode: Connectable) {
    addEdge(new ObjectFlow, otherNode)
  }
  
  def -->(otherNode: Connectable) {
    addEdge(new ControlFlow, otherNode)
  }
  
  private def addEdge(edge: ActivityEdge, otherNode: Connectable) {
    if (this.node != null && otherNode.node != null) {
      edge.source = this.node
      edge.target = otherNode.node
      node.activity.addEdge(edge)
    } else throw new IllegalArgumentException()
  }

}

case class ConnectableNode(nodeToConnect: Option[Object]) extends Connectable {
  override def wrapped = nodeToConnect
}