/*
 * Copyright (c) 2013 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.use.transform
import org.modelexecution.fuml.builder.FumlModelBuilder
import fUML.Semantics.Classes.Kernel.Object_
import fUML.Semantics.Classes.Kernel.Value

class UniversityManagementSystem extends FumlModelBuilder {

  val rootPackageName = "UniversityManagementSystem"
  val personClassName = "Person"
  val universityClassName = "University"
  val lectureClassName = "Lecture"
  val studentClassName = "Student"
  val studentStatusEnumName = "StudentStatus"
    
  packag(rootPackageName) withClassifiers (
      
      clazz(personClassName) withAttributes (
        attribute("givenName", STRING),
	    attribute("lastName", STRING)),

	  clazz(universityClassName),
	  
	  enum(studentStatusEnumName) withLiterals (
	      literal("active"),
	      literal("passive")),
	
	  clazz(lectureClassName) withAttributes (
	    attribute("lectureName", STRING),
	    attribute("lectureID", STRING),
	    attribute("ects", INTEGER),
	    attribute("prerequisiteECTS", INTEGER)),
	
	  clazz(studentClassName) withAttributes (
	    attribute("earnedECTS", INTEGER),
	    attribute("status", enum(studentStatusEnumName)))
	    extending(clazz(personClassName)),
	
	  association("offeredLectures") withProperties (
	    property("offeredLectures", clazz(lectureClassName), 1, -1),
	    property("offeringUniversity", clazz(universityClassName), 1, 1)),
	
	  association("attendedLectures") withProperties (
	    property("attendedLectures", clazz(lectureClassName), 0, -1),
	    property("attendingStudents", clazz(studentClassName), 0, -1))
	    
  )

  def valueScenario1 = {
    Set[Value](
      objectValue("uni", universityClass),

      objectValue("tanja", studentClass) withAttributeValues (
        featureValue(clazz(personClassName).attribute("givenName"), "Tanja"),
        featureValue(clazz(personClassName).attribute("lastName"), "Mayerhofer"),
        featureValue(clazz(studentClassName).attribute("earnedECTS"), 123),
        featureValue(clazz(studentClassName).attribute("status"), enum(studentStatusEnumName).literal("active"))),

      objectValue("me", lectureClass) withAttributeValues (
        featureValue(clazz(lectureClassName).attribute("lectureName"), "Model Engineering"),
        featureValue(clazz(lectureClassName).attribute("lectureID"), "199.999"),
        featureValue(clazz(lectureClassName).attribute("ects"), 12),
        featureValue(clazz(lectureClassName).attribute("prerequisiteECTS"), 100)),

      link("uni2me", association("offeredLectures")) withValues (
        featureValue(association("offeredLectures").property("offeredLectures"), getObject("me")),
        featureValue(association("offeredLectures").property("offeringUniversity"), getObject("uni"))),

      link("tanja2me", association("attendedLectures")) withValues (
        featureValue(association("attendedLectures").property("attendedLectures"), getObject("me")),
        featureValue(association("attendedLectures").property("attendingStudents"), getObject("tanja")))
    )
  }
  
  def rootPackage = getPackage(rootPackageName)
  def universityClass = getClass(universityClassName)
  def lectureClass = getClass(lectureClassName)
  def personClass = getClass(personClassName)
  def studentClass = getClass(studentClassName)
  def studentStatusEnum = getEnumeration(studentStatusEnumName)

}