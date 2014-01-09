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
import org.modelexecution.fuml.use.UniversityManagementSystem
import org.modelexecution.fuml.use.transform.FumlModel2UseModel
import org.modelexecution.fuml.use.transform.FumlValues2UseValues
import org.junit.Test
import org.tzi.use.uml.sys.MObject

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

    val modelFinder = new UseModelFinder(fUmlValues2UseValues.useSystem, objectConstraints)
  }

}