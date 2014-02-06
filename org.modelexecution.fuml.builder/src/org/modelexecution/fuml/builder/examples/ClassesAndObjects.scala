/*
 * Copyright (c) 2013 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package org.modelexecution.fuml.builder.examples

import org.modelexecution.fuml.builder.FumlModelBuilder

import fUML.Semantics.Classes.Kernel.Value

class ClassesAndObjects extends FumlModelBuilder {

  val rootPackageName = "ClassesAndObjects"
  val classClassName = "Class"
  val referenceClassName = "Reference"
  val attributeClassName = "Attribute"
  val objectClassName = "Object"
  val slotClassName = "Slot"
  val linkClassName = "Link"
    
  packag(rootPackageName) withClassifiers (
      
      clazz(classClassName) withAttributes (
        attribute("className", STRING),
	    attribute("abstract", BOOLEAN)),
	  
	  association("Generalization") withProperties (
	    property("general", clazz(classClassName), 0, -1),
	    property("specific", clazz(classClassName), 0, -1)),

	  clazz(referenceClassName) withAttributes(
	    attribute("refName", STRING)),
	  
	  association("References") withProperties (
	    property("source", clazz(classClassName), 1, 1),
	    property("references", clazz(referenceClassName), 0, -1)),
	  
	  association("ReferenceType") withProperties (
	    property("reference", clazz(referenceClassName), 0, -1),
	    property("referenceType", clazz(classClassName), 1, 1)),
	
	  clazz(attributeClassName) withAttributes (
	    attribute("attName", STRING)),
	    
	  association("Attributes") withProperties (
	    property("class", clazz(classClassName), 1, 1),
	    property("attributes", clazz(attributeClassName), 0, -1)),
	
	  clazz(objectClassName) withAttributes (
	    attribute("objectId", STRING)),
	
	  association("ObjectType") withProperties (
	    property("type", clazz(classClassName), 1, 1),
	    property("objects", clazz(objectClassName), 0, -1)),
	  
	  clazz(slotClassName) withAttributes (
	    attribute("attributeValue", STRING)),
	
	  association("ObjectSlots") withProperties (
	    property("object", clazz(objectClassName), 1, 1),
	    property("slots", clazz(slotClassName), 0, -1)),
	  
	  association("SlotAttribute") withProperties (
	    property("attribute", clazz(attributeClassName), 1, 1),
	    property("slots", clazz(slotClassName), 0, -1)),
	  
	  clazz(linkClassName),
	  
	  association("TargetObject") withProperties (
	    property("tlink", clazz(linkClassName), 0, -1),
	    property("target", clazz(objectClassName), 1, 1)),
	  
	  association("SourceObject") withProperties (
	    property("slink", clazz(linkClassName), 0, -1),
	    property("source", clazz(objectClassName), 1, 1)),
	    
	  association("LinkReference") withProperties (
	    property("links", clazz(linkClassName), 0, -1),
	    property("reference", clazz(referenceClassName), 1, 1)),
	    
	  activity("validateObject")
	    withInput(parameterNode("inputObject"))
	    withOutput(parameterNode("isValid"))
	    withNodes (
	      readFeatureNode("readSlots", association("ObjectSlots").property("slots"))
	    )
  )

  def personAddressScenario = {
    Set[Value](
      objectValue("PersonClass", classClass) withAttributeValues (
        featureValue(clazz(classClassName).attribute("className"), "Person"),
        featureValue(clazz(classClassName).attribute("abstract"), false)),

      objectValue("PersonName", attributeClass) withAttributeValues (
        featureValue(clazz(attributeClassName).attribute("attName"), "name")),
      
      link("PersonNameAttributeLink", association("Attributes")) withValues (
        featureValue(association("Attributes").property("class"), getObject("PersonClass")),
        featureValue(association("Attributes").property("attributes"), getObject("PersonName"))),
      
      objectValue("AddressClass", classClass) withAttributeValues (
        featureValue(clazz(classClassName).attribute("className"), "Address"),
        featureValue(clazz(classClassName).attribute("abstract"), false)),
      
      objectValue("AddressCity", attributeClass) withAttributeValues (
        featureValue(clazz(attributeClassName).attribute("attName"), "city")),
      
      link("AddressCityAttributeLink", association("Attributes")) withValues (
        featureValue(association("Attributes").property("class"), getObject("AddressClass")),
        featureValue(association("Attributes").property("attributes"), getObject("AddressCity"))),
        
      objectValue("PersonAddress", referenceClass) withAttributeValues (
        featureValue(clazz(referenceClassName).attribute("refName"), "address")),
      
      link("PersonAddressReferenceLink", association("References")) withValues (
        featureValue(association("References").property("source"), getObject("PersonClass")),
        featureValue(association("References").property("references"), getObject("PersonAddress"))),
        
      link("PersonAddressReferenceTypeLink", association("ReferenceType")) withValues (
        featureValue(association("ReferenceType").property("reference"), getObject("PersonAddress")),
        featureValue(association("ReferenceType").property("referenceType"), getObject("AddressClass")))
    )
  }
  
  def rootPackage = getPackage(rootPackageName)
  def referenceClass = getClass(referenceClassName)
  def attributeClass = getClass(attributeClassName)
  def classClass = getClass(classClassName)
  def objectClass = getClass(objectClassName)

}