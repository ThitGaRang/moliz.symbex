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
import org.modelexecution.fuml.symbexec._
import org.modelexecution.fuml.symbexec.FumlUtilities._
import scala.collection.JavaConversions._
import org.modelexecution.fuml.builder._
import fUML.Semantics.Classes.Kernel.Link
import fUML.Semantics.Classes.Kernel.Value

class ExecutionPathModelFinderTest extends TestCase {

  val model = new UniversityManagementSystem

  @Test
  def testFindingModelForSingleExistsFeatureValue {
    val fumlModel = FumlModel(model.rootPackage)
    val studentObject = createValue(model.studentClass)
    val pathCondition = ExistsFeatureValue(studentObject, model.attendedLecturesProperty)
    val executionStep = ExecutionStep(pathCondition, null)
    val executionTreeNode = ExecutionTreeNode(executionStep)
    val executionPath = ExecutionPath(executionTreeNode, true)

    val modelFinder = new ExecutionPathModelFinder(fumlModel)
    val result = modelFinder.findModel(executionPath, Set(studentObject))

    assertTrue(result.satisfiable)
    assertTrue(result.model.model(studentObject).isDefined)

    val studentObjectInModel = result.model.model(studentObject).get
    assertTrue(result.model.original(studentObjectInModel).isDefined)

    val studentObjectInResult = result.model.original(studentObjectInModel).get
    assertEquals(studentObject, studentObjectInResult)

    val attendedLecturesAssoc = model.attendedLecturesProperty.association

    val attendedLecturesLinks = result.model.values.collect {
      case link: Link if (attendedLecturesAssoc == link.`type`) => link
    }

    val linksReferringToStudent = refersToValue(studentObjectInModel, _: Link)
    val attendedLecturesLinksOfStudent = attendedLecturesLinks.filter(linksReferringToStudent)
    assertFalse(attendedLecturesLinksOfStudent.isEmpty)

    // TODO check how many students are generated
  }

  private def refersToValue(value: Value, link: Link) = {
    link.featureValues exists {
      _.values.exists {
        case reference: Reference => reference.referent == value
      }
    }
  }
  
}