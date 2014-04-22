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
import fUML.Semantics.Classes.Kernel.ExtensionalValue
import fUML.Syntax.Activities.IntermediateActivities.Activity
import fUML.Semantics.Classes.Kernel.Object_
import scala.collection.mutable.ListBuffer
import fUML.Semantics.Classes.Kernel.Value
import scala.collection.JavaConversions._
import ExecutionConfiguration._
import FumlUtilities._

class ExecutionConfiguration(val activity: Activity,
    binding: ParameterBindingMap = Map(),
    context: Option[Object_] = None) {

  private val symbolicInputValueBuffer = ListBuffer[ExtensionalValue]()

  val unboundParameters = {
    activity.ownedParameter.filter(inParameter).filterNot(binding.isDefinedAt).toList
  }

  val symbolicParameterValues = {
    unboundParameters.map { parameter =>
      val value = createValue(parameter.`type`)
      symbolicInputValueBuffer += value
      (parameter, List(value))
    }
  }

  val parameterBinding = {
    binding ++ symbolicParameterValues
  }

  val contextObject: Object_ = {
    context.getOrElse {
      val value = new Object_()
      symbolicInputValueBuffer += value
      value
    }
  }

  val symbolicInputValues = symbolicInputValueBuffer.toList
  
  val parameterValues = {
    parameterValueList(parameterBinding)
  }

}

object ExecutionConfiguration {
  type ParameterBindingMap = Map[Parameter, List[Value]]

  def apply(activity: Activity,
    binding: ParameterBindingMap = Map(),
    context: Option[Object_] = None) =
    new ExecutionConfiguration(activity, binding, context)
}