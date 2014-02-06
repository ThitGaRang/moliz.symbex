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
import fUML.Semantics.Classes.Kernel
import org.tzi.use.uml.ocl.value
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
import org.tzi.use.uml.mm.MModel
import org.tzi.use.api.UseSystemApi
import fUML.Semantics.Classes.Kernel.Object_
import fUML.Semantics.Classes.Kernel.Link
import fUML.Semantics.Classes.Kernel.FeatureValue
import fUML.Semantics.Classes.Kernel.ExtensionalValue
import fUML.Semantics.Classes.Kernel.ValueList
import fUML.Syntax.Classes.Kernel.StructuralFeature
import org.tzi.use.uml.mm.MAssociation
import org.tzi.use.uml.mm.MAssociationEnd
import fUML.Semantics.Classes.Kernel.Reference

class FumlValues2UseValues(val fUml2Use: FumlModel2UseModel)
  extends TracingOneToOneTransformation[Kernel.Value, Object] {
  import scala.collection.JavaConversions._

  type FumlValue = Kernel.Value

  val useSystemApi = UseSystemApi.create(fUml2Use.useModel)
  val useSystem = useSystemApi.getSystem()
  val useState = useSystem.state()

  override def children(value: FumlValue) = Seq()

  override protected def instantiate(fumlValue: FumlValue) = {
    fumlValue match {
      case fumlObject: Object_ => instantiateUseObject(fumlObject)
      case fumlLink: Link => instantiateUseLink(fumlLink)
      case _ => None
    }
  }

  private def instantiateUseObject(fumlObject: Object_) = {
    val objId = objectId(fumlObject)
    val useObject = useSystemApi.createObject(typeOf(fumlObject).name, objId)
    val featureValues = featureValueMap(fumlObject)
    featureValues.foreach {
      case (feature, values) =>
        val featureName = feature.name
        val value = stringRepresentation(feature, values)
        useSystemApi.setAttributeValue(objId, featureName, value)
    }
    Some(useObject)
  }

  private def stringRepresentation(feature: StructuralFeature,
    values: ValueList): String = {
    if (isStringAttribute(feature))
      stringRepresentation(values)
    else if (isEnumAttribute(feature))
      enumRepresentation(values)
    else
      attValueRepresentation(values)
  }

  private def isStringAttribute(feature: StructuralFeature) = {
    "String" == feature.typedElement.`type`.name
  }

  private def isEnumAttribute(feature: StructuralFeature) = {
    feature.typedElement.`type`.isInstanceOf[Enumeration]
  }

  private def stringRepresentation(values: ValueList) = {
    "'" + attValueRepresentation(values) + "'"
  }

  private def attValueRepresentation(values: ValueList) = {
    values.get(0).toString()
  }

  private def enumRepresentation(values: ValueList) = {
    "#" + attValueRepresentation(values)
  }

  private def featureValueMap(fumlValue: ExtensionalValue) = {
    fumlValue.featureValues.map(featureValueTupel).toMap
  }

  private def featureValueTupel(featureValue: FeatureValue) = {
    (featureValue.feature, featureValue.values)
  }

  private def objectId(obj: FumlValue): String = {
    obj match {
      case reference: Reference => objectId(reference.referent)
      case _ => "Obj_" + obj.hashCode()
    }
  }

  private def typeOf(fumlValue: FumlValue) = fumlValue.getTypes().get(0)

  private def instantiateUseLink(fumlLink: Link) = {
    transformLinkedObjects(fumlLink)
    val fumlAssociation = fumlLink.`type`
    val mAssociation = fUml2Use.use(fumlAssociation).asInstanceOf[MAssociation]
    val featureValues = featureValueMap(fumlLink)
    val linkedObjects = mAssociation.associationEnds().map { end =>
      findFeatureValueForEnd(fumlLink, end).values.getValue(0)
    }
    val linkedObjectIds = linkedObjects.map(objectId(_)).toArray
    val useLink = useSystemApi.createLink(typeOf(fumlLink).name, linkedObjectIds, Array[Array[String]]())
    Some(useLink)
  }

  private def transformLinkedObjects(fumlLink: Link) = {
    linkedObjects(fumlLink).foreach(transform(_))
  }

  private def linkedObjects(fumlLink: Link) = {
    featureValueValues(fumlLink).collect {
      case reference: Reference => reference.referent
      case obj: Object_ => obj
    }
  }

  private def featureValueValues(value: ExtensionalValue) = {
    value.featureValues.flatMap(_.values).toSeq
  }

  private def findFeatureValueForEnd(fumlLink: Link, end: MAssociationEnd) = {
    fumlLink.featureValues.find(featureValue => featureValue.feature.name == end.name()).get
  }

  def fuml(value: Object) = source(value)
  def use(value: FumlValue) = target(value)

}

object FumlValues2UseValues {
  def apply(fUml2Use: FumlModel2UseModel) = new FumlValues2UseValues(fUml2Use)
}