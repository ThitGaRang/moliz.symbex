package org.modelexecution.fuml.builder

import scala.collection.mutable
import org.modelexecution.fumldebug.core.util.ActivityFactory
import fUML.Syntax.Classes.Kernel.Property
import fUML.Syntax.Classes.Kernel.Class_
import fUML.Syntax.Classes.Kernel.Type
import org.modelexecution.fumldebug.core.ExecutionContext
import fUML.Syntax.Classes.Kernel.Association
import fUML.Syntax.Activities.IntermediateActivities.Activity
import fUML.Syntax.Activities.IntermediateActivities.ActivityNode
import fUML.Syntax.Activities.IntermediateActivities.InitialNode
import fUML.Syntax.Activities.IntermediateActivities.ControlFlow
import fUML.Syntax.Activities.IntermediateActivities.FinalNode
import fUML.Syntax.Activities.IntermediateActivities.ActivityFinalNode

trait FumlModelBuilder extends FumlModel {

  implicit def BuilderWrapper2Class_(builder: BuilderWrapper[Class_]) = builder.wrapped
  implicit def BuilderWrapper2Association(builder: BuilderWrapper[Association]) = builder.wrapped
  implicit def BuilderWrapper2Activity(builder: BuilderWrapper[Activity]) = builder.wrapped

  val STRING = ExecutionContext.getInstance().getPrimitiveStringType()
  val INTEGER = ExecutionContext.getInstance().getPrimitiveIntegerType()
  val BOOLEAN = ExecutionContext.getInstance().getPrimitiveBooleanType()

  def clazz(name: String) = {
    val clazz = classes.get(name).getOrElse(ActivityFactory.createClass(name))
    register[ClassBuilder](ClassBuilder(clazz))
  }

  def attribute(name: String, propertyType: Type,
    lower: Int = 0, upper: Int = 1, isUnique: Boolean = false) = {
    property(name, propertyType, lower, upper, isUnique)
  }

  def property(name: String, propertyType: Type,
    lower: Int = 0, upper: Int = 1, isUnique: Boolean = false) = {
    val property = new Property();
    property.setName(name)
    property.setType(propertyType)
    property.setLower(lower)
    property.setUpper(upper)
    property.setIsUnique(isUnique)
    property
  }

  def association(name: String) = {
    val association = associations.get(name).getOrElse(createAssociation(name))
    register[AssociationBuilder](AssociationBuilder(association))
  }

  private def createAssociation(name: String) = {
    val association = new Association
    association.name = name
    association
  }

  def activity(name: String) = {
    val activity = activities.get(name).getOrElse(ActivityFactory.createActivity(name))
    register[ActivityBuilder](ActivityBuilder(activity))
  }

  def initialNode(name: String) = {
    val node = new InitialNode
    node.name = name
    node
  }
  
  def activityFinalNode(name: String) = {
    val node = new ActivityFinalNode
    node.name = name
    node
  }

}

trait FumlModel {

  val classes: mutable.Map[String, Class_] = mutable.Map()
  val associations: mutable.Map[String, Association] = mutable.Map()
  val activities: mutable.Map[String, Activity] = mutable.Map()

  def register[T](element: T): T = {
    element match {
      case c: ClassBuilder => classes(c.clazz.name) = c.clazz
      case a: AssociationBuilder => associations(a.association.name) = a.association
      case a: ActivityBuilder => activities(a.activity.name) = a.activity
    }
    element
  }

}

sealed abstract class BuilderWrapper[T](val wrapped: T) {

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

}

case class ClassBuilder(clazz: Class_) extends BuilderWrapper[Class_](clazz) {

  def withAttributes(properties: Property*): ClassBuilder = {
    withAttributes(properties.toSet[Property])
  }

  def withAttributes(properties: Set[Property]): ClassBuilder = {
    properties.foreach(clazz.addOwnedAttribute(_))
    this
  }

  def attribute(name: String) = {
    clazz.ownedAttribute.toArray().find(isPropertyWithName(_, name))
  }

}

case class AssociationBuilder(association: Association) extends BuilderWrapper[Association](association) {

  def withProperties(properties: Property*): AssociationBuilder = {
    withProperties(properties.toSet[Property])
  }

  def withProperties(properties: Set[Property]): AssociationBuilder = {
    properties.foreach(association.memberEnd.add(_))
    this
  }

  def property(name: String) = {
    association.memberEnd.toArray().find(isPropertyWithName(_, name))
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

  def node(name: String) = {
    ConnectableNode(activity.node.toArray().find(isNodeWithName(_, name)))
  }

}

case class ConnectableNode(wrapped: Option[Object]) {

  val node = wrapped match {
    case w: Some[Object] => w.get match {
      case n: ActivityNode => n
      case _ => null
    }
    case _ => null
  }

  def -->(otherNode: ConnectableNode) {
    if (this.node != null && otherNode.node != null) {
      val edge = new ControlFlow
      edge.source = this.node
      edge.target = otherNode.node
      node.activity.addEdge(edge)
    } else throw new IllegalArgumentException()
  }

}