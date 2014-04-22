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

class ExecutionTreeTest extends TestCase {

  val model = new UniversityManagementSystem
  
  @Test
  def testFindingModelsForExecutionTree {
    val fumlModel = FumlModel(model.rootPackage)
    val studentObject = createValue(model.studentClass)
    val pathCondition = ExistsFeatureValue(studentObject, model.attendedLecturesProperty)
    val executionStep = ExecutionStep(pathCondition, null)
    val executionTree = ExecutionTreeNode(executionStep)
    
    
    
  }

}