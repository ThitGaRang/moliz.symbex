/*
 * Copyright (c) 2014 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.use.modelfinder.internal

import org.modelexecution.fuml.use.modelfinder.ModelElementBounds
import org.modelexecution.fuml.use.modelfinder.PrimitiveTypeBounds
import org.modelexecution.fuml.use.modelfinder.ModelFinderConfiguration
import org.tzi.kodkod.model.iface.IAssociation
import org.tzi.kodkod.model.iface.IAttribute
import org.tzi.kodkod.model.iface.IClass
import org.tzi.use.uml.mm.MAssociation
import org.tzi.use.uml.mm.MAttribute
import org.tzi.use.uml.mm.MClass
import org.tzi.use.uml.mm.MModel
import org.apache.commons.configuration.MapConfiguration
import org.apache.commons.configuration.Configuration

class KodkodConfigurator(configuration: ModelFinderConfiguration, model: MModel)
  extends PropertyConfigurationVisitor {
  import ModelFinderConfiguration._
  import ModelElementBounds._
  import collection.JavaConversions._

  val classes = model.classes().toSet
  val attributes = classes.flatMap(_.attributes()).toSet
  val associations = model.associations().toSet

  val classMinBounds = classes.map(minTuple(_)).toMap
  val classMaxBounds = classes.map(maxTuple(_)).toMap
  val classBounds = classMinBounds ++ classMaxBounds

  val attributeMinBounds = attributes.map(minTuple(_)).toMap
  val attributeMaxBounds = attributes.map(maxTuple(_)).toMap
  val attributeBounds = attributeMinBounds ++ attributeMaxBounds

  val associationMinBounds = associations.map(minTuple(_)).toMap
  val associationMaxBounds = associations.map(maxTuple(_)).toMap
  val associationBounds = associationMinBounds ++ associationMaxBounds

  val primitiveTypesMinBounds = PrimitiveTypes.map(minTuple(_)).toMap
  val primitiveTypesMaxBounds = PrimitiveTypes.map(maxTuple(_)).toMap
  val primitiveTypesBounds = primitiveTypesMinBounds ++ primitiveTypesMaxBounds

  val configMap = classBounds ++ attributeBounds ++ associationBounds ++ primitiveTypesBounds

  config = new MapConfiguration(configMap)

  private def minTuple(modelElement: Object) = {
    (name(modelElement) + "_min", minBound(modelElement))
  }

  private def maxTuple(modelElement: Object) = {
    (name(modelElement) + "_max", maxBound(modelElement))
  }

  private def name(modelElement: Object) = {
    modelElement match {
      case mClass: MClass => mClass.name()
      case mAtt: MAttribute => mAtt.owner().name + "_" + mAtt.name()
      case mAssoc: MAssociation => mAssoc.name()
      case string: String => string
    }
  }

  private def minBound(modelElement: Object) = {
    configuration.modelElementBounds collectFirst {
      case ModelElementBounds(`modelElement`, bounds) => bounds.min
    } getOrElse {
      modelElement match {
        case m: MClass => DefaultClassBounds.min
        case m: MAssociation => DefaultAssociationBounds.min
        case m: MAttribute => DefaultAttributeBounds.min
        case Real => configuration.primitiveTypeBounds.realBounds.min
        case String => configuration.primitiveTypeBounds.stringBounds.min
        case Integer => configuration.primitiveTypeBounds.intBounds.min
        case _ => 0
      }
    }
  }

  private def maxBound(modelElement: Object) = {
    configuration.modelElementBounds collectFirst {
      case ModelElementBounds(`modelElement`, bounds) => bounds.max
    } getOrElse {
      modelElement match {
        case m: MClass => DefaultClassBounds.max
        case m: MAssociation => DefaultAssociationBounds.max
        case m: MAttribute => DefaultAttributeBounds.max
        case Real => configuration.primitiveTypeBounds.realBounds.max
        case String => configuration.primitiveTypeBounds.stringBounds.max
        case Integer => configuration.primitiveTypeBounds.intBounds.max
        case _ => -1
      }
    }
  }

  private def mAssociation(association: IAssociation) = {
    model.getAssociation(association.name) match {
      case assoc: MAssociation => Some(assoc)
      case _ => None
    }
  }

  private def mAttribute(attribute: IAttribute, className: String) = {
    mClass(className).flatMap(findAttribute(_, attribute.name()))
  }

  private def mClass(className: String) = {
    model.getClass(className) match {
      case clazz: MClass => Some(clazz)
      case _ => None
    }
  }

  private def findAttribute(clazz: MClass, attributeName: String) = {
    clazz.attribute(attributeName, true) match {
      case attribute: MAttribute => Some(attribute)
      case _ => None
    }
  }

}

object KodkodConfigurator {
  def apply(configuration: ModelFinderConfiguration, model: MModel) = {
    new KodkodConfigurator(configuration, model)
  }
}