/*
 * Copyright (c) 2014 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.use.modelfinder

import org.tzi.use.uml.mm.MClass
import org.tzi.use.uml.mm.MAssociation
import org.tzi.use.uml.mm.MAttribute
import org.tzi.use.uml.mm.MModel

case class ModelFinderConfiguration(val modelElementBounds: Set[_ <: ModelElementBounds],
  val primitiveTypeBounds: PrimitiveTypeBounds = PrimitiveTypeBounds())

object ModelFinderConfiguration {
  import collection.JavaConversions._

  val String = "String"; val Real = "Real"; val Integer = "Integer"
  val PrimitiveTypes = Set(Real, Integer, String)

  def apply(model: MModel) = {
    val classes = model.classes().toSet
    val attributes = classes.flatMap(_.attributes()).toSet
    val associations = model.associations().toSet
    val allElements = classes ++ attributes ++ associations
    val modelElementBounds = allElements collect {
      case clazz: MClass => ClassBounds(clazz)
      case attribute: MAttribute => AttributeBounds(attribute)
      case association: MAssociation => AssociationBounds(association)
    }
    new ModelFinderConfiguration(modelElementBounds)
  }
}

case class PrimitiveTypeBounds(
  val intBounds: Bounds = PrimitiveTypeBounds.DefaultIntegerBounds,
  val realBounds: RealBounds = PrimitiveTypeBounds.DefaultRealBounds,
  val stringBounds: Bounds = PrimitiveTypeBounds.DefaultStringBounds)

object PrimitiveTypeBounds {
  val DefaultIntegerBounds = Bounds(-10, 10)
  val DefaultRealBounds = RealBounds(-2, 2, 0.5)
  val DefaultStringBounds = Bounds(0, 5)
}

sealed abstract class ModelElementBounds(
  val modelElement: Object, val bounds: Bounds)

case class Bounds(min: Int, max: Int)
case class RealBounds(min: Int, max: Int, step: Double)

object ModelElementBounds {
  val DefaultClassBounds = Bounds(0, 10)
  val DefaultAssociationBounds = Bounds(0, -1)
  val DefaultAttributeBounds = Bounds(0, -1)
  def unapply(modelElementBound: ModelElementBounds) = {
    modelElementBound match {
      case bound: ClassBounds => ClassBounds.unapply(bound)
      case bound: AssociationBounds => AssociationBounds.unapply(bound)
      case bound: AttributeBounds => AttributeBounds.unapply(bound)
    }
  }
}

case class ClassBounds(val mClass: MClass,
  override val bounds: Bounds = ModelElementBounds.DefaultClassBounds)
  extends ModelElementBounds(mClass, bounds)

case class AssociationBounds(val mAssociation: MAssociation,
  override val bounds: Bounds = ModelElementBounds.DefaultAssociationBounds)
  extends ModelElementBounds(mAssociation, bounds)

case class AttributeBounds(val mAttribute: MAttribute,
  override val bounds: Bounds = ModelElementBounds.DefaultAttributeBounds)
  extends ModelElementBounds(mAttribute, bounds)