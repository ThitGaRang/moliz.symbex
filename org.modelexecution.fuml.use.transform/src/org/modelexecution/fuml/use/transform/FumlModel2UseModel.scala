/*
 * Copyright (c) 2014 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.use.transform

import org.tzi.use.api.UseModelApi
import org.tzi.use.uml.mm.MModelElement
import fUML.Syntax.Classes.Kernel.Element
import fUML.Syntax.Classes.Kernel.Namespace
import fUML.Syntax.Classes.Kernel.Class_
import fUML.Syntax.Classes.Kernel.Association
import fUML.Syntax.Classes.Kernel.NamedElement
import fUML.Syntax.Classes.Kernel.Package
import fUML.Syntax.Classes.Kernel.Property
import fUML.Syntax.Classes.Kernel.AggregationKind
import org.tzi.use.uml.mm.MAggregationKind
import fUML.Syntax.Classes.Kernel.MultiplicityElement
import fUML.Syntax.Classes.Kernel.ElementList
import fUML.Syntax.Classes.Kernel.Enumeration
import org.tzi.use.uml.mm.MClass
import org.tzi.use.uml.mm.MClassifier
import org.tzi.use.uml.mm.MNamedElement

class FumlModel2UseModel(val modelName: String)
  extends TracingOneToOneTransformation[Element, MNamedElement] {
  import scala.collection.JavaConversions._

  val useApi = new UseModelApi
  val useModel = useApi.createModel(modelName)

  override def children(element: Element) = {
    element match {
      case p: Package => p.ownedType ++ p.nestedPackage
      case _ if element.ownedElement != null => element.ownedElement
      case _ => Seq()
    }
  }

  override protected def instantiate(element: Element) = {
    element match {
      case clazz: Class_ => instantiateClass(clazz)
      case enum: Enumeration => instantiateEnumeration(enum)
      case assoc: Association => instantiateAssociation(assoc)
      case property: Property => instantiateProperty(property)
      case _ => None
    }
  }

  private def instantiateClass(clazz: Class_) = {
    val mClass = useApi.createClass(clazz.name, clazz.isAbstract)
    addSuperTypes(clazz)
    Some(mClass)
  }

  private def addSuperTypes(clazz: Class_) {
    clazz.superClass.foreach {
      transform(_) match {
        case Some(mSuperClass) =>
          useApi.createGeneralization(clazz.name, mSuperClass.name())
        case None =>
      }
    }
  }

  private def instantiateEnumeration(enum: Enumeration) = {
    val mEnum = useApi.createEnumeration(enum.name, enum.ownedLiteral.map(_.name))
    Some(mEnum)
  }

  private def instantiateAssociation(assoc: Association) = {
    transformPropertyTypes(assoc.memberEnd.toSet)

    val memberEnds = assoc.memberEnd
    val classNames = toStringArray(memberEnds)(propertyType(_).name)
    val roleNames = toStringArray(memberEnds)(_.name)
    val multiplicities = toStringArray(memberEnds)(m => multiplicity(m.multiplicityElement))
    val aggregationKinds = toIntArray(memberEnds)(m => aggregationKind(m.aggregation))
    val orderedInfo = toBoolArray(memberEnds)(m => m.multiplicityElement.isOrdered)

    val mAssoc = useApi.createAssociation(assoc.name, classNames,
      roleNames, multiplicities, aggregationKinds, orderedInfo,
      Array[Array[Array[String]]]())

    Some(mAssoc)
  }

  private def transformPropertyTypes(properties: Set[Property]) = {
    properties.map(propertyType(_)).foreach(transform(_))
  }

  private def propertyType(property: Property) = property.typedElement.`type`

  private def toStringArray(properties: Seq[Property])(f: Property => String) = {
    properties.map(f(_)).toSeq.toArray
  }

  private def toIntArray(properties: Seq[Property])(f: Property => Int) = {
    properties.map(f(_)).toSeq.toArray
  }

  private def toBoolArray(properties: Seq[Property])(f: Property => Boolean) = {
    properties.map(f(_)).toSeq.toArray
  }

  private def aggregationKind(kind: AggregationKind) = {
    kind match {
      case AggregationKind.composite => MAggregationKind.COMPOSITION
      case AggregationKind.none => MAggregationKind.NONE
      case _ => MAggregationKind.AGGREGATION
    }
  }

  private def multiplicity(multiplicity: MultiplicityElement) = {
    val lower = multiplicity.lowerValue.toString()
    val upper = multiplicity.upperValue.toString()
    s"$lower..$upper"
  }

  private def instantiateProperty(property: Property) = {
    if (!isTransformed(property) && property.class_ != null) {
      transform(propertyType(property))
      val attributeName = property.name
      val ownerName = property.class_.name
      val typeName = propertyType(property).name
      val mProp = useApi.createAttribute(ownerName, attributeName, typeName)
      Some(mProp)
    } else {
      None
    }
  }

  def fuml(mElement: MModelElement) = source(mElement)
  def use(element: Element) = target(element)

}

object FumlModel2UseModel {
  def apply(modelName: String) = new FumlModel2UseModel(modelName)

  def apply(element: NamedElement): FumlModel2UseModel = {
    apply(element.name, element)
  }
  
  def apply(modelName: String, element: Element): FumlModel2UseModel = {
    apply(modelName, Set(element))
  }

  def apply(modelName: String, elements: Set[Element]) = {
    val fumlModel2UseModel = new FumlModel2UseModel(modelName)
    fumlModel2UseModel.transformAll(elements)
    fumlModel2UseModel
  }
}