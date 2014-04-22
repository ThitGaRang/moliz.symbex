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
import org.tzi.use.uml.sys.MObject
import org.tzi.use.uml.sys.MLink
import org.tzi.use.uml.mm.MAttribute
import fUML.Semantics.Classes.Kernel.FeatureValueList
import org.tzi.use.uml.ocl.`type`.EnumType
import org.tzi.use.uml.sys.MSystemState
import org.tzi.use.uml.sys.MLinkEnd
import fUML.Semantics.Classes.Kernel.Reference

class UseValues2FumlValues(val fUml2Use: FumlModel2UseModel, val useState: MSystemState)
  extends TracingOneToOneTransformation[Object, Kernel.Value] {
  import scala.collection.JavaConversions._

  type FumlValue = Kernel.Value
  type UseValue = value.Value

  override def children(value: Object) = Seq()

  override protected def instantiate(useObject: Object) = {
    useObject match {
      case useObject: MObject => instantiateFumlObject(useObject)
      case useLink: MLink => instantiateFumlLink(useLink)
      case _ => None
    }
  }

  private def instantiateFumlObject(useObject: MObject) = {
    val useObjectState = useObject.state(useState)
    val fumlClass = getFumlClass(useObject.cls())
    val fumlObject = new Object_()
    fumlObject.types.addValue(fumlClass)
    val useValueMap = useObjectState.attributeValueMap().toMap
    fumlObject.featureValues = toFeatureValues(useValueMap)
    Some(fumlObject)
  }

  private def getFumlClass(useClass: MClass) = {
    fUml2Use.fuml(useClass).asInstanceOf[Class_]
  }

  private def toFeatureValues(useValueMap: Map[MAttribute, UseValue]) = {
    val featureValueList = new FeatureValueList()
    useValueMap.foreach {
      case (useAttribute, useValue) =>
        val fUmlAttribute = getFumlAttribute(useAttribute)
        val featureValue = new FeatureValue
        featureValue.feature = fUmlAttribute
        featureValue.values = toAttributeValueList(useValue)
        featureValueList.add(featureValue)
    }
    featureValueList
  }

  private def getFumlAttribute(useAttribute: MAttribute) = {
    fUml2Use.fuml(useAttribute).asInstanceOf[Property]
  }

  private def toAttributeValueList(useValue: UseValue) = {
    useValue match {
      case value: value.CollectionValue => toCollectionValueList(value)
      case value => toValueList(toAttributeValue(value))
    }
  }

  private def toValueList(fumlValue: FumlValue) = {
    val valueList = new ValueList
    if (fumlValue != null) valueList.add(fumlValue)
    valueList
  }

  private def toCollectionValueList(useCollectionValue: value.CollectionValue) = {
    val valueList = new ValueList
    val values = useCollectionValue.collection().map { toAttributeValue(_) }
    valueList.addAll(values)
    valueList
  }

  private def toAttributeValue(useValue: UseValue): FumlValue = {
    useValue match {
      case value: value.BooleanValue => toBooleanValue(value)
      case value: value.EnumValue => toEnumValue(value)
      case value: value.IntegerValue => toIntegerValue(value)
      case value: value.RealValue => toRealValue(value)
      case value: value.StringValue => toStringValue(value)
      case value: value.ObjectValue => toObjectValue(value)
      case value: value.UndefinedValue => null
    }
  }

  private def toBooleanValue(useValue: value.BooleanValue) = {
    val value = new Kernel.BooleanValue
    value.value = useValue.value()
    value
  }

  private def toEnumValue(useValue: value.EnumValue) = {
    val useEnumType = useValue.`type`().asInstanceOf[EnumType]
    val fumlEnum = fUml2Use.allSourceElements.find {
      case fumlEnumElement: Enumeration => useEnumType.name() == fumlEnumElement.name
      case _ => false
    }.get.asInstanceOf[Enumeration]
    val value = new Kernel.EnumerationValue
    value.getTypes().add(fumlEnum)
    value.literal = fumlEnum.ownedLiteral.find(_.name == useValue.value()).get
    value
  }

  private def toIntegerValue(useValue: value.IntegerValue) = {
    val value = new Kernel.IntegerValue
    value.value = useValue.value()
    value
  }

  private def toRealValue(useValue: value.RealValue) = {
    null // not supported right now
  }

  private def toStringValue(useValue: value.StringValue) = {
    val value = new Kernel.StringValue
    value.value = useValue.value()
    value
  }

  private def toObjectValue(useValue: value.ObjectValue) = {
    transform(useValue.value()).get
  }

  private def instantiateFumlLink(useLink: MLink) = {
    val fumlAssociation = fUml2Use.fuml(useLink.association()).asInstanceOf[Association]
    val featureValues = useLink.linkEnds().map { toFeatureValue(_) }
    val fumlLink = new Link()
    fumlLink.`type` = fumlAssociation
    fumlLink.featureValues.addAll(featureValues)
    Some(fumlLink)
  }

  private def toFeatureValue(useLinkEnd: MLinkEnd) = {
    val useAssociation = useLinkEnd.associationEnd().association()
    val fumlAssociation = fUml2Use.fuml(useAssociation).asInstanceOf[Association]
    val assocEndName = useLinkEnd.associationEnd().name()
    val fumlProperty = fumlAssociation.memberEnd.find(_.name == assocEndName).get
    val linkedUseObject = useLinkEnd.`object`()
    val featureValue = new FeatureValue()
    featureValue.feature = fumlProperty
    featureValue.values.add(toFumlReference(linkedUseObject))
    featureValue
  }
  
  private def toFumlReference(useObject: MObject) = {
    val reference = new Reference()
    reference.referent = transform(useObject).get.asInstanceOf[Object_]
    reference
  }

  def fuml(value: Object) = target(value)
  def use(value: FumlValue) = source(value)

}

object UseValues2FumlValues {
  def apply(fUml2Use: FumlModel2UseModel,
      useState: MSystemState) = new UseValues2FumlValues(fUml2Use, useState)
}