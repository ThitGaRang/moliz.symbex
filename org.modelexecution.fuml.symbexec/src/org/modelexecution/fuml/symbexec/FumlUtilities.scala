/*
 * Copyright (c) 2014 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.symbexec

import fUML.Syntax.Classes.Kernel.Parameter
import fUML.Syntax.Classes.Kernel.ParameterDirectionKind
import fUML.Syntax.Classes.Kernel.Type
import fUML.Syntax.Classes.Kernel.Class_
import fUML.Semantics.Classes.Kernel.Object_
import fUML.Syntax.Activities.IntermediateActivities.Activity
import scala.collection.JavaConversions._
import fUML.Semantics.Classes.Kernel.Reference
import fUML.Semantics.CommonBehaviors.BasicBehaviors.ParameterValue
import fUML.Semantics.CommonBehaviors.BasicBehaviors.ParameterValueList
import fUML.Semantics.Classes.Kernel.Value
import fUML.Syntax.Classes.Kernel.StructuralFeature
import fUML.Syntax.Classes.Kernel.MultiplicityElement

object FumlUtilities {

  def parameter(activity: Activity, name: String) = {
    activity.ownedParameter.find(name == _.name).get
  }

  def inParameter(parameter: Parameter): Boolean = {
    parameter.direction == ParameterDirectionKind.in ||
      parameter.direction == ParameterDirectionKind.inout
  }

  def parameterValueList(values: Map[Parameter, Traversable[Value]]) = {
    val parameterValueList = new ParameterValueList
    parameterValueList.addAll(values.map(entry => paramterValue(entry._1, entry._2)))
    parameterValueList
  }

  def paramterValue(parameter: Parameter, values: Traversable[Value]) = {
    val parameterValue = new ParameterValue
    parameterValue.parameter = parameter
    for (value <- values) parameterValue.values.addValue(referent(value))
    parameterValue
  }

  def referent(value: Value) = {
    value match {
      case obj: Object_ => reference(obj)
      case _ => value
    }
  }

  def reference(value: Object_) = {
    val reference = new Reference
    reference.referent = value
    reference
  }

  def createValue(fumlType: Type) = {
    fumlType match {
      // TODO support other types such as enumerations, etc.
      case classType: Class_ => {
        val obj = new Object_()
        obj.types.addValue(classType)
        obj
      }
    }
  }
  
  def isMultivalued(feature: StructuralFeature): Boolean =
    isMultivalued(feature.multiplicityElement)
  
  def isMultivalued(multiplicityElement: MultiplicityElement): Boolean = {
    multiplicityElement.upper.naturalValue match {
      case 1 => false
      case _ => true
    }
  }

}