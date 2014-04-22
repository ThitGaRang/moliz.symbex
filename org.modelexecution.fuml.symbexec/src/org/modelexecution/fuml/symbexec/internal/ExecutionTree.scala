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

import org.modelexecution.fumldebug.core.trace.tracemodel.ActivityNodeExecution
import fUML.Semantics.Classes.Kernel.Value
import fUML.Syntax.Classes.Kernel.StructuralFeature
import org.modelexecution.fuml.symbexec.FumlUtilities

sealed trait ExecutionTree {
  var parent: Option[ExecutionTree] = None
}

case object UnsatisfiableNode extends ExecutionTree
case object UnexploredNode extends ExecutionTree

case class ExecutionTreeNode(step: ExecutionStep,
  var falseBranch: ExecutionTree = UnexploredNode,
  var trueBranch: ExecutionTree = UnexploredNode)
    extends ExecutionTree {

  def executionSteps: Seq[ExecutionStep] = {
    val parentsExecutionSteps = parent.map {
      case node: ExecutionTreeNode => node.executionSteps
      case _ => Seq()
    }
    parentsExecutionSteps.getOrElse(Seq()) ++ Seq(this.step)
  }

}

case class ExecutionStep(
    condition: PathCondition,
    nodeExecution: ActivityNodeExecution,
    symbolicState: Map[Value, String] = Map()) {
  def context = condition.context
  def constraint = condition.constraint
  // TODO replace somehow symbolic variables based on symbolic state
}

case class ExecutionPath(executionTreeNode: ExecutionTreeNode, branch: Boolean) {
  val executionSteps: Seq[ExecutionStep] = executionTreeNode.executionSteps
}

sealed trait PathCondition {
  val context: Value
  def constraint: String
}

case class ExistsFeatureValue(context: Value, feature: StructuralFeature)
    extends PathCondition {
  override def constraint = {
    val featureName = feature.name
    FumlUtilities.isMultivalued(feature) match {
      case true => s"not self.$featureName->isEmpty()"
      case false => s"self.$featureName <> OCLUndefined"
    }
  }
}

case class OclExpression(context: Value, expression: String)
    extends PathCondition {
  override def constraint = expression
}