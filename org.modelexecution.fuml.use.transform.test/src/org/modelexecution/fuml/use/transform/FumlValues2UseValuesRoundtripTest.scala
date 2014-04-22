package org.modelexecution.fuml.use.transform

import org.junit.Assert._
import org.junit.Test
import org.tzi.use.uml.mm.MAssociation
import org.tzi.use.uml.mm.MAttribute
import org.tzi.use.uml.mm.MClass
import org.tzi.use.uml.ocl.`type`.EnumType
import fUML.Syntax.Classes.Kernel.Association
import fUML.Syntax.Classes.Kernel.Class_
import fUML.Syntax.Classes.Kernel.Property
import junit.framework.TestCase
import org.tzi.use.uml.sys.MObject
import org.tzi.use.uml.sys.MLink
import org.tzi.use.uml.sys.MSystemState
import fUML.Semantics.Classes.Kernel
import fUML.Semantics.Classes.Kernel.Object_
import fUML.Semantics.Classes.Kernel.Link
import org.tzi.use.uml.sys.MObjectState
import org.tzi.use.uml.ocl.value
import fUML.Semantics.Classes.Kernel.ExtensionalValue
import org.modelexecution.fuml.builder.examples.UniversityManagementSystem
import fUML.Semantics.Classes.Kernel.Reference

class FumlValues2UseValuesRoundtripTest extends TestCase {
  import scala.collection.JavaConversions._

  type UseValue = value.Value
  type FumlValue = Kernel.Value

  val model = new UniversityManagementSystem

  val universityClass = model.universityClass
  val lectureClass = model.lectureClass
  val personClass = model.personClass
  val studentClass = model.studentClass
  val studentStatusEnum = model.studentStatusEnum

  var state: MSystemState = null

  @Test def testTransformingUniversityManagementSystem {
    val fUml2UseModel = FumlModel2UseModel(model.rootPackage)

    // transform fUML 2 USE -- RUN 1
    val fUmlValues2UseValues = FumlValues2UseValues(fUml2UseModel)
    state = fUmlValues2UseValues.useState
    val fUmlValues = model.valueScenario1
    val useValues = fUmlValues2UseValues.transformAll(model.valueScenario1).map(_.get)

    // transform USE back to fUML -- Roundtrip
    val useValues2FumlValues = UseValues2FumlValues(fUml2UseModel, fUmlValues2UseValues.useState)
    val fUmlValues2 = useValues2FumlValues.transformAll(useValues).map(_.get)

    // transform resulting fUML again to USE -- RUN 2
    val fUmlValues2UseValues2 = FumlValues2UseValues(fUml2UseModel)
    val useValues2 = fUmlValues2UseValues2.transformAll(fUmlValues2).map(_.get)
    state = fUmlValues2UseValues2.useState

    assertEquals(useValues.size, useValues2.size)

    // compare original fUML values to result of fUML->USE->fUML->USE
    fUmlValues.foreach { value =>
      val useValue2 = fUmlValues2UseValues2.use(useValues2FumlValues.fuml(fUmlValues2UseValues.use(value)))
      assertEqualsValue(value, useValue2)
    }
  }

  private def assertEqualsValue(fumlValue: FumlValue, useObject: Object) = {
    fumlValue match {
      case fumlObject: Object_ =>
        assertEqualsObject(fumlObject, useObject.asInstanceOf[MObject].state(state))
      case fumlLink: Link =>
        assertEqualsLink(fumlLink, useObject.asInstanceOf[MLink])
    }
  }

  private def assertEqualsObject(fumlObject: Object_, useObject: MObjectState) = {
    val useClass = useObject.`object`().cls()
    assertEquals(fumlObject.types.getValue(0).name, useClass.name())
    useClass.allAttributes().foreach { useAtt =>
      val useAttValue = useObject.attributeValue(useAtt)
      val fumlAttValue = featureValue(fumlObject, useAtt.name()).getValue(0)
      assertEquals(string(fumlAttValue), string(useAttValue))
    }
  }

  def featureValue(fumlValue: ExtensionalValue, name: String) = {
    fumlValue.featureValues.find(featureValue => name == featureValue.feature.name).get.values
  }

  def string(fumlValue: FumlValue) = {
    fumlValue.toString()
  }

  def string(useValue: UseValue) = {
    useValue match {
      case enumValue: value.EnumValue => enumValue.value()
      case stringValue: value.StringValue => stringValue.value()
      case _ => useValue.toString()
    }
  }

  private def assertEqualsLink(fumlLink: Link, useLink: MLink) = {
    assertEquals(fumlLink.`type`.name, useLink.association().name())
    val useAssoc = useLink.association()
    val useLinkedObjects = useLink.linkedObjects()
    useAssoc.associationEnds().zipWithIndex.foreach {
      case (useMemberEnd, index) =>
        val useLinkedObject = useLinkedObjects.get(index)
        val fumlFeatureValue = featureValue(fumlLink, useMemberEnd.name()).getValue(0)
        assertEqualsObject(toObject(fumlFeatureValue), useLinkedObject.state(state))
    }
  }

  private def toObject(value: FumlValue) = {
    value match {
      case reference: Reference => reference.referent
      case _ => value.asInstanceOf[Object_]
    }
  }

  private def debugPrint(obj: Object, state: MSystemState) {
    obj match {
      case mObject: MObject => debugPrint(mObject, state)
      case mLink: MLink => debugPrint(mLink, state)
    }
    println("====")
  }

  private def debugPrint(useObject: MObject, state: MSystemState) {
    val useClass = useObject.cls()
    val useObjState = useObject.state(state)
    println(useObject + ":" + useClass.name())
    useClass.allAttributes().foreach { att =>
      println(" " + att.name() + "=" + useObjState.attributeValue(att))
    }
  }

  private def debugPrint(useLink: MLink, state: MSystemState) {
    val useAssoc = useLink.association()
    val linkedObjects = useLink.linkedObjects()
    println(useLink + ":" + useAssoc.name())
    useAssoc.associationEnds().zipWithIndex.foreach {
      case (end, index) =>
        println(" " + end.name() + "=" + linkedObjects.get(index))
    }
  }

}