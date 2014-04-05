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

import fUML.Syntax.Classes.Kernel.Parameter
import fUML.Syntax.Classes.Kernel.ParameterDirectionKind

object FumlUtilities {
  
  def inParameter(parameter: Parameter): Boolean = {
    parameter.direction == ParameterDirectionKind.in ||
      parameter.direction == ParameterDirectionKind.inout
  }

}