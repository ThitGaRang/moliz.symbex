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
import org.junit.Test
import org.modelexecution.fuml.builder.examples.ClassesAndObjects
import org.modelexecution.fuml.use.transform.FumlModel2UseModel
import org.modelexecution.fuml.use.transform.FumlValues2UseValues
import org.tzi.use.uml.sys.MObject
import org.tzi.use.uml.sys.MSystem
import junit.framework.TestCase
import org.tzi.use.uml.mm.MClass

class UseModelFinderTest extends TestCase {

  val model = new ClassesAndObjects

  @Test def testTransformingUniversityManagementSystem {
    val fUml2UseModel = FumlModel2UseModel(model.rootPackage)
    val fUmlValues2UseValues = FumlValues2UseValues(fUml2UseModel)
    val fUmlValues = model.personAddressScenario

    fUmlValues2UseValues.transformAll(fUmlValues)

    // avoid generating additional classes, references, and attributes
    val useClassClass = fUml2UseModel.use(model.classClass).asInstanceOf[MClass]
    val useReferenceClass = fUml2UseModel.use(model.referenceClass).asInstanceOf[MClass]
    val useAttributeClass = fUml2UseModel.use(model.attributeClass).asInstanceOf[MClass]
    val useObjectClass = fUml2UseModel.use(model.objectClass).asInstanceOf[MClass]
    val configuration = ModelFinderConfiguration(
      Set(
        ClassBounds(useClassClass, Bounds(2, 2)),
        ClassBounds(useAttributeClass, Bounds(2, 2)),
        ClassBounds(useReferenceClass, Bounds(1, 1)),
        ClassBounds(useObjectClass, Bounds(0, 2))
        ))

    val personClassObject = fUmlValues2UseValues.use(model.getObject("PersonClass")).asInstanceOf[MObject]

    val objectConstraints = Set[UseObjectConstraint]( 
        UseObjectConstraint(personClassObject, "self.objects->exists(o | o.type = self and o.slink->forAll(l | self.references->exists(r | r.referenceType = l.target.type)))")
    )

    val modelFinder = new UseModelFinder(fUmlValues2UseValues.useSystem, objectConstraints)

    val result = modelFinder.findModel(configuration)

    println(result)
    println("===============")
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
      println(obj + " : " + obj.cls().name())
      println("------")
      println(objectState.attributeValueMap())
      println
    }
  }

  private def printLinks(useSystem: MSystem) {
    val linkIter = useSystem.state().allLinks().iterator()
    while (linkIter.hasNext()) {
      val link = linkIter.next()
      println(link.association())
      println("------")
      for (linkedObj <- link.linkedObjectsAsArray().array) {
        println(linkedObj)
      }
      println
    }
  }

}