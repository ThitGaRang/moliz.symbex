package org.modelexecution.fuml.use.transform

import scala.collection.JavaConversions.asScalaBuffer

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import org.modelexecution.fuml.builder.examples.UniversityManagementSystem
import org.tzi.use.uml.mm.MAssociation
import org.tzi.use.uml.mm.MAttribute
import org.tzi.use.uml.mm.MClass
import org.tzi.use.uml.ocl.`type`.EnumType

import fUML.Syntax.Classes.Kernel.Association
import fUML.Syntax.Classes.Kernel.Class_
import fUML.Syntax.Classes.Kernel.Property
import junit.framework.TestCase

class FumlModel2UseModelTest extends TestCase {
  import scala.collection.JavaConversions._

  val model = new UniversityManagementSystem

  val universityClass = model.universityClass
  val lectureClass = model.lectureClass
  val personClass = model.personClass
  val studentClass = model.studentClass
  val studentStatusEnum = model.studentStatusEnum

  @Test def testTransformingUniversityManagementSystem {
    val fUml2UseModel = FumlModel2UseModel(model.rootPackageName)
    fUml2UseModel.transform(model.rootPackage)

    assertEquals(universityClass.name, fUml2UseModel.target(universityClass).name())
    assertEquals(lectureClass.name, fUml2UseModel.target(lectureClass).name())
    assertEquals(personClass.name, fUml2UseModel.target(personClass).name())
    assertEquals(studentClass.name, fUml2UseModel.target(studentClass).name())
    assertEquals(studentStatusEnum.name, fUml2UseModel.target(studentStatusEnum).name())

    val mPersonClass = fUml2UseModel.target(personClass).asInstanceOf[MClass]
    val mStudentClass = fUml2UseModel.target(studentClass).asInstanceOf[MClass]
    assertTrue(mStudentClass.parents().contains(mPersonClass))

    assertEquals(fUml2UseModel.use(studentClass.ownedAttribute.get(0)), mStudentClass.attributes().get(0))
    assertEquals(fUml2UseModel.use(studentClass.ownedAttribute.get(1)), mStudentClass.attributes().get(1))

    assertTrue(fUml2UseModel.allSourceElements.forall { sourceElement =>
      sourceElement eq fUml2UseModel.source(fUml2UseModel.target(sourceElement))
    })

    assertTrue(fUml2UseModel.allSourceElements.forall { sourceElement =>
      sourceElement match {
        case clazz: Class_ => equals(clazz, fUml2UseModel.target(clazz).asInstanceOf[MClass])
        case assoc: Association => equals(assoc, fUml2UseModel.target(assoc).asInstanceOf[MAssociation])
        case _ => true
      }
    });

    val mStudentStatusEnumType = fUml2UseModel.target(studentStatusEnum).asInstanceOf[EnumType]
    assertTrue(mStudentStatusEnumType.getLiterals().contains("active"))
    assertTrue(mStudentStatusEnumType.getLiterals().contains("passive"))

  }

  private def equals(clazz: Class_, mClass: MClass): Boolean = {
    clazz.name == mClass.name() && clazz.ownedAttribute.size() == mClass.attributes().size() &&
      clazz.ownedAttribute.forall(att =>
        mClass.attribute(att.name, false) != null && equals(att, mClass.attribute(att.name, false)))
  }

  private def equals(property: Property, mProperty: MAttribute): Boolean = {
    property.name == mProperty.name()
  }

  private def equals(assoc: Association, mAssoc: MAssociation): Boolean = {
    assoc.name == mAssoc.name() && assoc.memberEnd.size() == mAssoc.associationEnds().size()
  }

}