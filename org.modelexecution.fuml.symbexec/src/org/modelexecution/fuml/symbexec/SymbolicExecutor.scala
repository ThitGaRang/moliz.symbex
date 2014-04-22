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

import fUML.Syntax.Activities.IntermediateActivities.ActivityParameterNode
import fUML.Semantics.Classes.Kernel.Value
import fUML.Syntax.Activities.IntermediateActivities.Activity
import fUML.Syntax.Classes.Kernel.Element
import fUML.Syntax.Classes.Kernel.StructuralFeature
import fUML.Syntax.Classes.Kernel.NamedElement
import org.modelexecution.fuml.use.transform.FumlModel2UseModel
import org.modelexecution.fuml.use.transform.FumlValues2UseValues
import org.modelexecution.fumldebug.core.trace.tracemodel.ActivityNodeExecution
import org.modelexecution.fumldebug.core.ExecutionContext
import fUML.Semantics.Classes.Kernel.Object_
import fUML.Semantics.Classes.Kernel.ExtensionalValue
import org.modelexecution.fumldebug.core.ExecutionEventListener
import org.modelexecution.fumldebug.core.event.Event
import fUML.Syntax.Classes.Kernel.Property
import fUML.Semantics.Activities.IntermediateActivities.ObjectToken
import fUML.Syntax.Classes.Kernel.Parameter
import fUML.Syntax.Classes.Kernel.ParameterDirectionKind
import fUML.Syntax.Classes.Kernel.ParameterList
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import fUML.Syntax.Classes.Kernel.TypedElement
import fUML.Syntax.Classes.Kernel.Type
import org.modelexecution.fumldebug.core.event.ActivityNodeEntryEvent
import org.modelexecution.fumldebug.core.event.ActivityEntryEvent
import scala.collection.mutable
import org.modelexecution.fumldebug.core.event.ActivityExitEvent
import org.modelexecution.fumldebug.core.trace.tracemodel.Trace
import fUML.Syntax.Actions.IntermediateActions.ReadStructuralFeatureAction
import fUML.Syntax.Actions.BasicActions.Action
import org.modelexecution.fumldebug.core.trace.tracemodel.ActionExecution
import org.modelexecution.fumldebug.core.trace.tracemodel.DecisionNodeExecution
import org.modelexecution.fumldebug.core.trace.tracemodel.Input
import org.modelexecution.fumldebug.core.event.ActivityNodeExitEvent
import org.modelexecution.fumldebug.core.trace.tracemodel.InputValue
import org.modelexecution.fumldebug.core.trace.tracemodel.TokenInstance
import org.modelexecution.fumldebug.core.trace.tracemodel.ObjectTokenInstance

import org.modelexecution.fuml.symbexec.internal._

import fUML.Syntax.Classes.Kernel.Class_
import scala.collection.JavaConversions._
import SymbolicExecutor._

class SymbolicExecutor(fumlModel: FumlModel, values: FumlValues = Set()) {

  val fumlModel2UseModel = fumlModel.fumlModel2UseModel
  val fumlValues2UseValues = FumlValues2UseValues(fumlModel2UseModel)
  val fumlExecutionCtx = ExecutionContext.getInstance()

  val executionIds = new mutable.Stack[Int]()
  var config: ExecutionConfiguration = null
  var executionTree: ExecutionTree = null
  var currentTreeNode: ExecutionTreeNode = null
  var currentBranch: Boolean = true

  def execute(config: ExecutionConfiguration): ExecutionTree = {
    initialize(config)

    // TODO implement
    fumlExecutionCtx.execute(config.activity,
      config.contextObject, config.parameterValues)

    uninitializeExecutionCtx
    executionTree
  }

  private def initialize(config: ExecutionConfiguration): Unit = {
    initializeExecutionCtx
    executionIds.clear
    this.config = config
  }

  private def initializeExecutionCtx: Unit = {
    fumlExecutionCtx.reset()
    fumlExecutionCtx.getExtensionalValues().addAll(values)
    fumlExecutionCtx.addEventListener(eventHandler)
  }

  private def uninitializeExecutionCtx: Unit = {
    fumlExecutionCtx.removeEventListener(eventHandler)
    fumlExecutionCtx.reset()
  }

  val eventHandler = new ExecutionEventListener() {
    override def notify(event: Event) {
      event match {
        case e: ActivityEntryEvent => executionIds.push(e.getActivityExecutionID())
        case e: ActivityExitEvent => executionIds.pop
        case e: ActivityNodeEntryEvent => processNodeEntry(e)
        case e: ActivityNodeExitEvent =>
        // TODO we could check if we went as expected
        // TODO update symbolic state
        case _ =>
      }
    }
  }

  private def processNodeEntry(event: ActivityNodeEntryEvent): Unit = {
    currentNodeExecution(executionTrace) match {
      case actionExec: ActionExecution if (concernsSymbolicValue(actionExec)) =>
        processAction(actionExec)
      case decisionExec: DecisionNodeExecution if (concernsSymbolicValue(decisionExec)) =>
        println("decision to be handled") // TODO implement
      case _ =>
    }
  }

  private def executionTrace = fumlExecutionCtx.getTrace(executionId)

  private def executionId = {
    executionIds.isEmpty match {
      case false => executionIds.last
      case true => -1
    }
  }

  private def currentNodeExecution(trace: Trace) =
    trace.getActivityExecutions().last.getNodeExecutions().last

  private def concernsSymbolicValue(actionExec: ActionExecution) =
    actionExec.getInputs() exists (isSymbolicInput)

  private def concernsSymbolicValue(nodeExec: DecisionNodeExecution) =
    hasSymbolicDecisionInputValue(nodeExec) || hasSymbolicRoutedInputValue(nodeExec)

  private def hasSymbolicDecisionInputValue(nodeExec: DecisionNodeExecution) =
    Option(nodeExec.getDecisionInputValue()) exists (isSymbolicInputValue)

  private def hasSymbolicRoutedInputValue(nodeExec: DecisionNodeExecution) =
    nodeExec.getRoutedTokens() exists (isSymbolicObjectTokenInstance)

  private def isSymbolicObjectTokenInstance(tokenInstance: TokenInstance) = {
    tokenInstance match {
      case objectToken: ObjectTokenInstance =>
        isSymbolicValue(objectToken.getTransportedValue().getRuntimeValue())
      case _ => false
    }
  }

  private def isSymbolicInput(input: Input): Boolean =
    input.getInputValues() exists (isSymbolicInputValue)

  private def isSymbolicInputValue(inputValue: InputValue) =
    isSymbolicValue(inputValue.getInputValueSnapshot().getRuntimeValue())

  private def isSymbolicValue(value: Value): Boolean = {
    // TODO also check if it is a value that depends on a symbolic input
    config.symbolicInputValues.contains(value)
  }

  private def processAction(actionExec: ActionExecution): Unit = {
    val updatedExecTree = updateExecutionTree(actionExec)
    if (updatedExecTree) {
      // TODO
      // ask execution strategy
      // execute model finder for true or false path (according to strategy)
      // update runtime model
      // continue/restart execution
    }
  }

  // Returns true if the execution tree has been updated
  private def updateExecutionTree(actionExec: ActionExecution) = {
    actionExec.getNode() match {
      case _: ReadStructuralFeatureAction =>
        updateExecutionTreeReadStructuralFeatureAction(actionExec); true
      case _ => false
    }
  }

  // TODO maybe move that into an execution tree builder
  private def updateExecutionTreeReadStructuralFeatureAction(actionExec: ActionExecution) = {
    val inputValues = actionExec.getInputs().head.getInputValues()
    val firstInputValue = inputValues.filter(isSymbolicInputValue).toList.head
    val symbolicValue = firstInputValue.getInputValueSnapshot().getRuntimeValue()
    val feature = actionExec.getNode().asInstanceOf[ReadStructuralFeatureAction].structuralFeature

    val pathCondition = ExistsFeatureValue(symbolicValue, feature)
    val executionStep = ExecutionStep(pathCondition, actionExec)

    addExecutionTreeNode(ExecutionTreeNode(executionStep))
  }

  private def addExecutionTreeNode(treeNode: ExecutionTreeNode): Unit = {
    if (currentTreeNode == null) {
      currentTreeNode = treeNode
      executionTree = currentTreeNode
    } else {
      if (currentBranch == true) currentTreeNode.trueBranch = treeNode
      if (currentBranch == false) currentTreeNode.falseBranch = treeNode
      treeNode.parent = Some(currentTreeNode)
      currentTreeNode = treeNode
    }
  }

}

object SymbolicExecutor {
  type FumlValues = Set[_ <: ExtensionalValue]
  def apply(fumlModel: FumlModel, values: FumlValues = Set()): SymbolicExecutor =
    new SymbolicExecutor(fumlModel, values)
}

trait SymbolicExecutionStrategy {
  def pick(currentTreeNode: ExecutionTreeNode): Option[Boolean]
  // specific implementations might need access to
  // Activity and a work list (nodes to execute)
  // TODO introduce continue method
}

case class SymbolicValue(
  value: Value, var concreteProperties: Set[Property] = Set[Property]())

case class FumlModel(name: String = "Anonymous", elements: Set[Element]) {
  val fumlModel2UseModel = FumlModel2UseModel(name, elements)
  val useModel = fumlModel2UseModel.useModel
}

object FumlModel {
  def apply(element: NamedElement): FumlModel =
    FumlModel(element.name, Set(element))
}

