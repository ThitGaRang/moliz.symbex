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
import fUML.Syntax.Classes.Kernel.Class_

class SymbolicExecutor(context: SymbolicExecutionContext) {
  import scala.collection.JavaConversions._

  val fumlModel2UseModel = context.fumlModel.fumlModel2UseModel
  val fumlValues2UseValues = FumlValues2UseValues(fumlModel2UseModel)
  val fumlExecutionCtx = ExecutionContext.getInstance()

  var executionPath: ExecutionPath = null
  var executionTree: ExecutionTree = null

  def execute(activity: Activity,
    binding: ParameterBinding = ParameterBinding(),
    contextObject: Object_ = null) = {
    initialize

    // fill in missing parameters
    val unboundParameters = binding.unboundParameters(activity.ownedParameter)
    val symParameterValues = unboundParameters.map{ para =>
      (para, List(createValue(para.`type`)))
    }
    val filledParameterBinding = ParameterBinding(binding.binding ++ symParameterValues)
    
    // track symbolic input values
    val symbolicInput = symParameterValues.map {
      case (_, List(value)) => value
    }
    // TODO we should consider contextObject symbolic if null

    // TODO implement

    uninitializeExecutionCtx
  }
  
  
  private def createValue(fumlType: Type) = {
    fumlType match {
      // TODO support other types such as enumerations, etc.
      case classType: Class_ => {
    	  val obj = new Object_()
    	  obj.types.addValue(classType)
    	  obj
      }
    }
  }
  

  private def initialize {
    initializeExecutionCtx
    executionPath = ExecutionPath()
  }

  private def initializeExecutionCtx {
    fumlExecutionCtx.reset()
    fumlExecutionCtx.getExtensionalValues().addAll(context.values)
    fumlExecutionCtx.addEventListener(eventHandler)
  }

  private def uninitializeExecutionCtx {
    fumlExecutionCtx.removeEventListener(eventHandler)
    fumlExecutionCtx.reset()
  }

  val eventHandler = new ExecutionEventListener() {
    override def notify(event: Event) {
      // TODO implement
      println("this is the symbex: " + event)
    }
  }

}

object SymbolicExecutor {

  def apply(context: SymbolicExecutionContext) =
    new SymbolicExecutor(context)

  def apply(fumlModel: FumlModel,
    values: Set[_ <: ExtensionalValue] = Set()): SymbolicExecutor =
    SymbolicExecutor(SymbolicExecutionContext(fumlModel, values))

}

case class ParameterBinding(binding: Map[Parameter, List[_ <: ExtensionalValue]] = Map()) {
  import FumlUtilities._
  def unboundParameters(parameters: Traversable[Parameter]) = {
    parameters.filter(inParameter).filterNot(binding.isDefinedAt).toList
  }
}

trait SymbolicExecutionStrategy {
  def pick(path: ExecutionPath, tree: ExecutionTree): Option[Boolean]
  // specific implementations might need access to
  // Activity and a work list (nodes to execute)
}

case class SymbolicExecutionContext(
  fumlModel: FumlModel,
  values: Set[_ <: ExtensionalValue])

case class SymbolicValue(
  value: ExtensionalValue,
  var concreteProperties: Set[Property] = Set[Property]())

case class FumlModel(name: String = "Anonymous", elements: Set[Element]) {
  val fumlModel2UseModel = FumlModel2UseModel(name, elements)
  val useModel = fumlModel2UseModel.useModel
}

object FumlModel {
  def apply(element: NamedElement): FumlModel =
    FumlModel(element.name, Set(element))
}

sealed trait PathCondition
case class ExistentialPathCondition extends PathCondition
case class PathExpression extends PathCondition // inputs, decision behavior, output

case class ExecutionPath(var steps: Seq[ExecutionStep] = Seq())
case class ExecutionStep(
  condition: PathCondition,
  nodeExecution: ActivityNodeExecution,
  symbolicState: Map[Value, String] = Map())

sealed trait ExecutionTree
case object Unsatisfiable extends ExecutionTree
case object Unexplored extends ExecutionTree
case class ExecutionTreeNode(step: ExecutionStep,
  var falseBranch: ExecutionTree,
  var trueBranch: ExecutionTree)
    extends ExecutionTree
