package org.modelexecution.fuml.modelfinder.sandbox

import java.io.File

import scala.collection.JavaConversions.collectionAsScalaIterable

import org.tzi.kodkod.KodkodModelValidatorConfiguration
import org.tzi.kodkod.model.config.impl.ModelConfigurator
import org.tzi.kodkod.model.config.impl.PropertyConfigurationVisitor
import org.tzi.kodkod.model.iface.IInvariant
import org.tzi.kodkod.model.iface.IModel
import org.tzi.use.api.UseModelApi
import org.tzi.use.api.UseSystemApi
import org.tzi.use.kodkod.UseKodkodModelValidator
import org.tzi.use.kodkod.plugin.PluginModelFactory
import org.tzi.use.uml.mm.MAggregationKind
import org.tzi.use.uml.sys.MSystem

object UseModelFinderSandbox extends App {
  import scala.collection.JavaConversions._

  override def main(args: Array[String]) {
    val api = new UseModelApi

    val model = api.createModel("PersonKnowingPersonModel")
    api.createClass("Person", false)
    api.createAttribute("Person", "name", "String")
    api.createAttribute("Person", "ects", "Integer")

    api.createClass("Lecture", false)
    api.createAttribute("Lecture", "name", "String")
    api.createAttribute("Lecture", "ects", "Integer")

    api.createAssociation("PersonDidLectures",
      "Lecture", "lectures", "2..*", MAggregationKind.NONE,
      "Person", "person", "1", MAggregationKind.NONE)

    api.createInvariant("TestInv", "Person", "self.lectures->select(l | self.ects >= l.ects)->isEmpty()", false)
    //api.createInvariant("TestInv", "Lecture", "self.ects > 0 and self.ects < self.person.ects", false)

    val system = UseSystemApi.create(model)
    system.createObject("Person", "tanja")
    system.setAttributeValue("tanja", "name", "'Tanja'")
    system.setAttributeValue("tanja", "ects", "50")
    // system.createObject("Person", "philip")
    // system.setAttributeValue("philip", "name", "'Philip'")
    //system.createLink("PersonKnowsPerson", "tanja", "philip")

    // println(system.evaluate("Person.allInstances()->select(name = 'Tanja')"))

    tryGenerator(system)
  }

  def tryGenerator(systemApi: UseSystemApi) {
    val system = systemApi.getSystem()

    val model = system.model
    val iModel = PluginModelFactory.INSTANCE.getModel(model)

    KodkodModelValidatorConfiguration.INSTANCE.getModelEnricher().enrichModel(system, iModel)
    enrichModelWithLoadedInvariants(system, iModel)

    val defaultConfigFile = new File("test.properties")
    //    val defaultConfigVisitor = new DefaultConfigurationVisitor("test.use")
    //    val defaultConfigFile = defaultConfigVisitor.getFile()
    //    iModel.accept(defaultConfigVisitor)

    val propertyConfigurator = new PropertyConfigurationVisitor(defaultConfigFile.getAbsolutePath()) {
      override def visitInvariant(invariant: IInvariant) {
        println("invariant " + invariant + " will be activated")
        invariant.activate()
      }
    }
    iModel.accept(propertyConfigurator);

    val modelValidator = new UseKodkodModelValidator(system)

    modelValidator.validate(iModel)

    val objectIter = system.state().allObjectNames().iterator()
    while (objectIter.hasNext()) {
      val obj = system.state().objectByName(objectIter.next())
      val objectState = obj.state(system.state())
      println(obj)
      println(objectState.attributeValueMap())
    }

    val linkIter = system.state().allLinks().iterator()
    while (linkIter.hasNext()) {
      val link = linkIter.next()
      println(link.association())
      for (linkedObj <- link.linkedObjectsAsArray().array) {
        println(linkedObj)
      }
    }

    println(systemApi.evaluate("Person.allInstances()->forAll(p | p.ects > p.lectures->collect(ects)->sum())"))
    println(systemApi.evaluate("Person.allInstances()->asSequence()->at(1).lectures->collect(ects)->sum()"))
  }

  def enrichModelWithLoadedInvariants(mSystem: MSystem, model: IModel) {
    // TODO check how they get invariants here to work - in our case the invariant in Person is missing
    val configurator = model.getConfigurator().asInstanceOf[ModelConfigurator]
    configurator.clear()
    val invariantTransformator = new InvariantTransformator(model.modelFactory(), model.typeFactory());
    
    for (inv <- mSystem.model().classInvariants()) {
      val transformedInv = invariantTransformator.transform(model, inv)
      configurator.addInvariant(transformedInv)
    }
  }

}