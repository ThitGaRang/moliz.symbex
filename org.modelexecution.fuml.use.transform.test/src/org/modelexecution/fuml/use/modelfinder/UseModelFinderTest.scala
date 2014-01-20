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

import org.junit.Assert._

import junit.framework.TestCase

import org.modelexecution.fuml.use.examples.UniversityManagementSystem
import org.modelexecution.fuml.use.modelfinder.UseModelFinder;
import org.modelexecution.fuml.use.transform.FumlModel2UseModel
import org.modelexecution.fuml.use.transform.FumlValues2UseValues
import org.junit.Test
import org.tzi.use.uml.sys.MObject
import org.tzi.use.uml.sys.MSystem
import org.tzi.use.uml.mm.MClass

class UseModelFinderTest extends TestCase {

  val model = new UniversityManagementSystem

  val universityClass = model.universityClass
  val lectureClass = model.lectureClass
  val personClass = model.personClass
  val studentClass = model.studentClass
  val studentStatusEnum = model.studentStatusEnum

  @Test def testTransformingUniversityManagementSystem {
    val fUml2UseModel = FumlModel2UseModel(model.rootPackage)
    val fUmlValues2UseValues = FumlValues2UseValues(fUml2UseModel)
    val state = fUmlValues2UseValues.useState
    val fUmlValues = model.valueScenario1
    fUmlValues2UseValues.transformAll(model.valueScenario1).map(_.get)

    val tanjaObject = fUmlValues2UseValues.use(model.getObject("tanja")).asInstanceOf[MObject]
    val meObject = fUmlValues2UseValues.use(model.getObject("me")).asInstanceOf[MObject]

    val objectConstraints = Set[UseObjectConstraint](
      UseObjectConstraint(tanjaObject, "self.attendedLectures->exists(l | self.earnedECTS > l.ects)"),
      UseObjectConstraint(tanjaObject, "self.attendedLectures->exists(l | self.lastName <> l.lectureName)"),
      UseObjectConstraint(meObject, "self.ects > 1 and self.ects < 16"))

    val useStudentClass = fUml2UseModel.use(studentClass).asInstanceOf[MClass]

    val configuration = ModelFinderConfiguration(
      Set(ClassBounds(useStudentClass, Bounds(1, 10))))
    val modelFinder = new UseModelFinder(fUmlValues2UseValues.useSystem, objectConstraints)

    modelFinder.findModel(configuration)

    printObjectSpace(fUmlValues2UseValues.useSystem)
  }

  private def printObjectSpace(useSystem: MSystem) {
    printObjects(useSystem)
    printLinks(useSystem)
  }

  private def printObjects(useSystem: MSystem) {
    val objectIter = useSystem.state().allObjectNames().iterator()
    while (objectIter.hasNext()) {
      val obj = useSystem.state().objectByName(objectIter.next())
      val objectState = obj.state(useSystem.state())
      println(obj)
      println(objectState.attributeValueMap())
    }
  }

  private def printLinks(useSystem: MSystem) {
    val linkIter = useSystem.state().allLinks().iterator()
    while (linkIter.hasNext()) {
      val link = linkIter.next()
      println(link.association())
      for (linkedObj <- link.linkedObjectsAsArray().array) {
        println(linkedObj)
      }
    }
  }

}