/*
 * Copyright (c) 2014 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.use.transform

import scala.collection.mutable.HashMap

abstract trait OneToOneTransformation[A, B] extends Traverser[A] {

  protected def instantiate(a: A): Option[B]

  def transformAll(as: Set[A]) = {
    as map { transform(_) }
  }

  def transform(a: A): Option[B] = {
    val b = instantiate(a)
    traverse(a) { (transform(_)) }
    b
  }

}

abstract trait Traverser[A] {

  def children(a: A) = Seq[A]()

  def traverse(a: A)(f: A => Unit) {
    children(a).foreach(f)
  }
}

trait BidirectionalOneToOneTrace[A, B] {

  private val a2b = HashMap[A, B]()
  private val b2a = HashMap[B, A]()

  def trace(a: A, b: B) {
    a2b(a) = b
    b2a(b) = a
  }
  
  def allSourceElements() = a2b.keys
  def allTargetElements() = b2a.keys

  def source(b: B) = b2a(b)

  def target(a: A) = a2b(a)

  def hasTrace(obj: Any) = {
    a2b.contains(obj.asInstanceOf[A]) || b2a.contains(obj.asInstanceOf[B])
  }

}

abstract trait TracingOneToOneTransformation[A, B]
  extends OneToOneTransformation[A, B] with BidirectionalOneToOneTrace[A, B] {

  override def transform(a: A) = {
    if (hasTrace(a)) {
      Some(target(a))
    } else {
      val b = super.transform(a)
      b match {
        case Some(b) => trace(a, b)
        case None =>
      }
      b
    }
  }
  
  def isTransformed(a: A) = hasTrace(a)

}