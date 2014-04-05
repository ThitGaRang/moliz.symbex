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

import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import org.modelexecution.fuml.builder.examples.UniversityManagementSystem
import org.modelexecution.fumldebug.core.ExecutionContext
import org.modelexecution.fumldebug.core.ExecutionEventListener
import org.modelexecution.fumldebug.core.event.Event
import fUML.Semantics.Classes.Kernel.Object_
import fUML.Syntax.Classes.Kernel.Parameter
import fUML.Semantics.CommonBehaviors.BasicBehaviors.ParameterValueList
import fUML.Semantics.CommonBehaviors.BasicBehaviors.ParameterValue
import fUML.Semantics.Classes.Kernel.Reference
import org.modelexecution.fumldebug.core.event.ActivityEntryEvent
import fUML.Syntax.Activities.IntermediateActivities.Activity

class SymbolicExecutorTest extends TestCase {
  import scala.collection.JavaConversions._

  val model = new UniversityManagementSystem
  var executionIds = List[Int]()
  
  @Test
  def testSymbolicExecutionReadAttendedLecturesNoConcreteInput {
    val symbolicExecutor = SymbolicExecutor(FumlModel(model.rootPackage))
    symbolicExecutor.execute(model.readAttendedLectures)
    // TODO elaborate
  }

  @Test
  def testConcreteExecutionReadAttendedLectures {
    val fumlExecutionCtx = initializedExecutionContext()

    val tanjaValue = model.getObject("tanja")
    val meValue = model.getObject("me")

    val activity = model.readAttendedLectures
    val parameterValues = toParameterValueList(
      Map((tanjaValue, parameter(activity, "inStudent"))))

    fumlExecutionCtx.execute(activity, null, parameterValues)

    val output = fumlExecutionCtx.getActivityOutput(executionIds.head)
    val firstResultObject = output.getValue(0).values.get(0).asInstanceOf[Reference].referent

    assertEquals(meValue, firstResultObject)
  }

  private def initializedExecutionContext() = {
    val fumlExecutionCtx = ExecutionContext.getInstance()
    fumlExecutionCtx.reset()
    fumlExecutionCtx.getExtensionalValues().addAll(model.valueScenario1)
    fumlExecutionCtx.addEventListener(newEventListener(println(_)))
    fumlExecutionCtx.addEventListener(newEventListener(saveExecutionId(_)))
    fumlExecutionCtx
  }

  private def parameter(activity: Activity, name: String) = {
    activity.ownedParameter.find(name == _.name).get
  }

  private def toParameterValueList(values: Map[Object_, Parameter]) = {
    val parameterValueList = new ParameterValueList
    parameterValueList.addAll(values.map(entry => toParamterValue(entry._1, entry._2)))
    parameterValueList
  }

  private def toParamterValue(value: Object_, parameter: Parameter) = {
    val parameterValue = new ParameterValue
    parameterValue.parameter = parameter
    parameterValue.values.addValue(toReference(value))
    parameterValue
  }

  private def toReference(value: Object_) = {
    val reference = new Reference
    reference.referent = value
    reference
  }

  private def newEventListener(action: (Event => Unit)) = {
    new ExecutionEventListener() {
      override def notify(event: Event) {
        action(event)
      }
    }
  }

  private def saveExecutionId(event: Event) {
    event match {
      case activityEntryEvent: ActivityEntryEvent =>
        executionIds = List(activityEntryEvent.getActivityExecutionID()) ++ executionIds
      case _ =>
    }
  }

}