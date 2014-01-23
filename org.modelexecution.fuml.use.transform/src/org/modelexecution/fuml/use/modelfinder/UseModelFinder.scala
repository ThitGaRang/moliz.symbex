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

import java.io.PrintWriter

import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.seqAsJavaList

import org.modelexecution.fuml.use.modelfinder.internal.InvariantTransformator
import org.modelexecution.fuml.use.modelfinder.internal.KodkodConfigurator
import org.tzi.kodkod.model.`type`.PrimitiveTypeFactory
import org.tzi.kodkod.model.config.impl.ModelConfigurator
import org.tzi.kodkod.model.iface.IModel
import org.tzi.kodkod.model.impl.SimpleFactory
import org.tzi.kodkod.ocl.OCLGroupRegistry
import org.tzi.kodkod.ocl.operation.AnyOperationGroup
import org.tzi.kodkod.ocl.operation.BooleanOperationGroup
import org.tzi.kodkod.ocl.operation.ClassOperationGroup
import org.tzi.kodkod.ocl.operation.CollectionConstructorGroup
import org.tzi.kodkod.ocl.operation.ConditionalOperationGroup
import org.tzi.kodkod.ocl.operation.IntegerOperationGroup
import org.tzi.kodkod.ocl.operation.SetOperationGroup
import org.tzi.kodkod.ocl.operation.VariableOperationGroup
import org.tzi.use.kodkod.UseKodkodModelValidator
import org.tzi.use.kodkod.transform.ModelTransformator
import org.tzi.use.kodkod.transform.enrich.ObjectDiagramModelEnricher
import org.tzi.use.parser.SrcPos
import org.tzi.use.parser.Symtable
import org.tzi.use.parser.ocl.OCLCompiler
import org.tzi.use.uml.mm.MClass
import org.tzi.use.uml.mm.ModelFactory
import org.tzi.use.uml.ocl.`type`.ObjectType
import org.tzi.use.uml.ocl.expr.Expression
import org.tzi.use.uml.sys.MObject
import org.tzi.use.uml.sys.MSystem

// TODO optimization
// could be changed so that constructor only takes model
// and findModel takes system state, to save memory and transformation time

class UseModelFinder(val useSystem: MSystem,
  val objectConstraints: Set[UseObjectConstraint]) {
  import collection.JavaConversions._

  val model = useSystem.model()
  val systemState = useSystem.state()
  val allObjects = systemState.allObjects()
  val allLinks = systemState.allLinks()

  private val simpleFactory = new SimpleFactory()
  private val typeFactory = new PrimitiveTypeFactory()
  private val modelFactory = new ModelFactory()
  private val iModel = new ModelTransformator(simpleFactory, typeFactory).transform(model)
  private val iModelConfigurator = iModel.getConfigurator().asInstanceOf[ModelConfigurator]

  protected val errorPrintWriter = new PrintWriter(System.err)

  registerDefaultOperationGroups()

  private def registerDefaultOperationGroups() {
    val registry = OCLGroupRegistry.INSTANCE;
    registry.registerOperationGroup(new VariableOperationGroup(typeFactory));
    registry.registerOperationGroup(new IntegerOperationGroup(typeFactory));
    registry.registerOperationGroup(new BooleanOperationGroup(typeFactory));
    registry.registerOperationGroup(new ClassOperationGroup(typeFactory));
    registry.registerOperationGroup(new AnyOperationGroup(typeFactory, true));
    registry.registerOperationGroup(new ConditionalOperationGroup(typeFactory));
    registry.registerOperationGroup(new SetOperationGroup(typeFactory));
    registry.registerOperationGroup(new CollectionConstructorGroup(typeFactory));
  }

  def addInvariant(invName: String, invBody: String,
    mClass: MClass, isExistential: Boolean = false) = {
    val invExp = compileExpression(invBody, mClass)
    val classInv = createClassInvariant(invName, invExp, mClass, isExistential)
    model.addClassInvariant(classInv)
  }

  private def compileExpression(invBody: String, mClass: MClass) = {
    OCLCompiler.compileExpression(model, invBody, "UseModelFinder",
      errorPrintWriter, symtableWithObjectVariables(mClass.`type`()))
  }

  private def symtableWithObjectVariables(typeOfSelf: ObjectType) = {
    val symtable = newSymtable(typeOfSelf)
    allObjects.foreach { addObjectVariable(_, symtable) }
    symtable
  }

  private def newSymtable(typeOfSelf: ObjectType) = {
    val symtable = new Symtable()
    symtable.add("self", typeOfSelf, new SrcPos("self", 1, 1))
    symtable
  }

  private def addObjectVariable(obj: MObject, symtable: Symtable) {
    val srcPos = new SrcPos(objectVariableName(obj), 1, 1)
    symtable.add(objectVariableName(obj), objectType(obj), srcPos)
  }

  private def createClassInvariant(invName: String, invExp: Expression,
    mClass: MClass, isExistential: Boolean) = {
    modelFactory.createClassInvariant(invName, variableNames,
      mClass, invExp, isExistential)
  }

  private def variableNames() = {
    allObjects.map(objectVariableName).toList
  }

  private def objectType(obj: MObject) = {
    obj.cls().`type`()
  }

  private def objectVariableName(obj: MObject) = {
    obj.name()
  }

  def findModel(configuration: ModelFinderConfiguration) = {
    installObjectConstraintsInModel
    configureIModel(configuration)
    val validator = new InternalUseKodkodModelValidator(useSystem)
    validator.validate(iModel)
    removeObjectConstraints
    validator.foundValidSolution
  }

  private def installObjectConstraintsInModel() {
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

  private def constraintsPerObject() = {
    constrainedObjects.map(obj => (obj, constraintsOfObject(obj))).toMap
  }

  private def constrainedObjects() = {
    objectConstraints.map(_.mObject)
  }

  private def constraintsOfObject(obj: MObject) = {
    objectConstraints.collect {
      case objConstraint: UseObjectConstraint if obj == objConstraint.mObject =>
        objConstraint.constraint
    }
  }

  private def configureIModel(configuration: ModelFinderConfiguration) {
    iModelConfigurator.clear()
    iModel.accept(KodkodConfigurator(configuration, model))
    enrichIModelWithObjects
    enrichIModelWithInvariants
  }

  private def enrichIModelWithInvariants() {
    val invariantTransformator = new InvariantTransformator(simpleFactory, typeFactory)
    for (inv <- model.classInvariants()) {
      val transformedInv = invariantTransformator.transform(iModel, inv)
      iModelConfigurator.addInvariant(transformedInv)
    }
  }

  private def enrichIModelWithObjects() {
    val modelEnricher = new ObjectDiagramModelEnricher()
    modelEnricher.enrichModel(useSystem, iModel)
  }

  private def removeObjectConstraints() {
    allObjects.foreach { obj =>
      model.classInvariants().remove(obj.name())
    }
  }

}

case class UseObjectConstraint(mObject: MObject, constraint: String)

class InternalUseKodkodModelValidator(useSystem: MSystem)
  extends UseKodkodModelValidator(useSystem) {

  object ModelFindingResult extends Enumeration {
    type ModelFindingResult = Value
    val Satisfiable, TriviallySatisfiable = Value
    val Unsatisfiable, TriviallyUnsatisfiable = Value
  }

  import ModelFindingResult._

  var result: ModelFindingResult = Unsatisfiable

  def foundValidSolution() = (result == Satisfiable || result == TriviallySatisfiable)

  override def satisfiable() {
    result = Satisfiable
    super.satisfiable()
  }

  override def trivially_satisfiable() {
    result = TriviallySatisfiable
    super.trivially_satisfiable()
  }

  override def unsatisfiable() {
    result = Unsatisfiable
    super.unsatisfiable()
  }

  override def trivially_unsatisfiable() {
    result = TriviallyUnsatisfiable
    super.trivially_unsatisfiable
  }
}