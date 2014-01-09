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

import fUML.Syntax.Classes.Kernel.Namespace
import org.modelexecution.fuml.use.transform.FumlModel2UseModel
import org.tzi.use.api.UseSystemApi
import org.tzi.use.uml.sys.MSystem
import org.tzi.use.uml.mm.MModel
import org.tzi.use.uml.sys.MObject
import org.tzi.use.uml.mm.ModelFactory
import org.tzi.use.parser.Symtable
import org.tzi.use.parser.SrcPos
import org.tzi.use.uml.ocl.`type`.ObjectType
import org.tzi.use.uml.mm.MClass
import org.tzi.use.parser.ocl.OCLCompiler
import java.io.PrintWriter
import org.tzi.use.uml.ocl.expr.Expression

class UseModelFinder(val useSystem: MSystem,
  val objectConstraints: Set[UseObjectConstraint]) {
  import scala.collection.JavaConversions._

  val model = useSystem.model()
  val systemState = useSystem.state()

  protected val errorPrintWriter = new PrintWriter(System.err)

  private val MODELFINDER = "ModelFinder"
  private val modelFactory = new ModelFactory()

  installObjectConstraints()

  private def installObjectConstraints() {
    constraintsPerObject map {
      case (obj, constraints) =>
        val objectVar = objectVariableName(obj)
        val combinedConstraint = constraints.reduce(_ + " and " + _)
        (obj, s"self = $objectVar implies ($combinedConstraint)")
    } foreach {
      case (obj, objectConstraint) =>
        addInvariant(obj.name(), objectConstraint, obj.cls(), false)
    }
  }

  private def constrainedObjects = {
    objectConstraints.map(_.mObject)
  }

  private def constraintsPerObject = {
    constrainedObjects.map(obj => (obj, constraintsOfObject(obj))).toMap
  }

  private def constraintsOfObject(obj: MObject) = {
    objectConstraints.collect {
      case objConstraint: UseObjectConstraint if obj == objConstraint.mObject =>
        objConstraint.constraint
    }
  }

  def addInvariant(invName: String, invBody: String,
    mClass: MClass, isExistential: Boolean = false) = {
    val invExp = compileExpression(invBody, mClass)
    val classInv = createClassInvariant(invName, invExp, mClass, isExistential)
    model.addClassInvariant(classInv)
  }

  private def varSymtable(typeOfSelf: ObjectType) = {
    val symtable = new Symtable()
    symtable.add("self", typeOfSelf, new SrcPos("self", 1, 1))
    allObjects().foreach { obj =>
      val srcPos = new SrcPos(objectVariableName(obj), 1, 1)
      symtable.add(objectVariableName(obj), objectType(obj), srcPos)
    }
    symtable
  }

  private def compileExpression(invBody: String, mClass: MClass) = {
    val variables = varSymtable(mClass.`type`())
    OCLCompiler.compileExpression(model, invBody, MODELFINDER, errorPrintWriter, variables)
  }

  private def createClassInvariant(invName: String, invExp: Expression,
    mClass: MClass, isExistential: Boolean) = {
    modelFactory.createClassInvariant(invName, variableNames,
      mClass, invExp, isExistential)
  }

  private def variableNames = allObjects().map(objectVariableName).toList

  private def objectType(obj: MObject) = obj.cls().`type`()

  protected def objectVariableName(obj: MObject) = obj.name()
  
  // TODO allow to set configurations
  // therefore subclass PropertyConfigurationVisitor
  
  def findModel = {
    // TODO find model and return true if satisfiable
    // therefore we might have to subclass UseKodkodModelValidator
    true
  }

  def allObjects() = systemState.allObjects()
  def allLinks() = systemState.allLinks()

}

case class UseObjectConstraint(mObject: MObject, constraint: String)