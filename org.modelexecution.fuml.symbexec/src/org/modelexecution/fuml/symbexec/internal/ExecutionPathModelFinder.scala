/*
 * Copyright (c) 2014 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.symbexec.internal

import org.modelexecution.fuml.symbexec.FumlModel
import fUML.Semantics.Classes.Kernel.Value
import org.modelexecution.fuml.use.transform.FumlValues2UseValues
import org.modelexecution.fuml.use.modelfinder.ModelFinderConfiguration
import org.modelexecution.fuml.use.modelfinder.UseObjectConstraint
import org.modelexecution.fuml.use.modelfinder.UseModelFinder
import org.modelexecution.fuml.use.transform.UseValues2FumlValues
import org.tzi.use.uml.sys.StateChangeListener
import org.tzi.use.uml.sys.MSystemState
import org.tzi.use.uml.sys.MObject
import fUML.Syntax.Classes.Kernel.StructuralFeature

import scala.collection.JavaConversions._

class ExecutionPathModelFinder(fumlModel: FumlModel) {

  private val fumlModel2UseModel = fumlModel.fumlModel2UseModel
  private val fumlValues2UseValues = FumlValues2UseValues(fumlModel2UseModel)
  private val useModel = fumlModel.useModel
  private val useSystem = fumlValues2UseValues.useSystem

  def findModel(executionPath: ExecutionPath, values: Set[Value]) = {
    fumlValues2UseValues.transformAll(values)

    val executionSteps = executionPath.executionSteps
    val objectConstraints = executionSteps.map(toUseObjectConstraints)
    
    val modelFinder = new UseModelFinder(useSystem, objectConstraints)
    val result = modelFinder.findModel(ModelFinderConfiguration(useModel))

    val useValues2FumlValues = UseValues2FumlValues(fumlModel2UseModel, currentUseState)
    useValues2FumlValues.transformAll(currentObjectsAndLinks).map(_.get)

    ModelFindingResult(result, FoundModel(fumlValues2UseValues, useValues2FumlValues))
  }

  private def toUseObjectConstraints(executionStep: ExecutionStep) = {
      val context = mObject(executionStep.context)
      val constraint = executionStep.constraint
      UseObjectConstraint(context, constraint)
  }

  private def mObject(value: Value) =
    fumlValues2UseValues.use(value).asInstanceOf[MObject]

  private def currentUseState =
    fumlValues2UseValues.useState.system().state()

  private def currentObjectsAndLinks = {
    val currentState = currentUseState
    val allObjects = currentState.allObjects().toSet
    val allLinks = currentState.allLinks().toSet
    allObjects ++ allLinks
  }

}

case class FoundModel(values: Set[Value], originalToModel: Map[Value, Value]) {
  val modelToOriginal = originalToModel map { case (o, e) => (e, o) }
  def original(value: Value) = modelToOriginal.get(value)
  def model(originalValue: Value) = originalToModel.get(originalValue)
}

object FoundModel {
  def apply(fuml2UseValues: FumlValues2UseValues,
    use2FumlValues: UseValues2FumlValues): FoundModel = {
    val values: Set[Value] = use2FumlValues.allTargetElements.toSet
    val correspondences = use2FumlValues.allSourceElements.collect {
      case useObject: MObject if (fuml2UseValues.hasTrace(useObject)) =>
        (fuml2UseValues.fuml(useObject), use2FumlValues.fuml(useObject))
    }
    val originalToModel: Map[Value, Value] = correspondences.toMap
    FoundModel(values, originalToModel)
  }
}

case class ModelFindingResult(satisfiable: Boolean, model: FoundModel)
