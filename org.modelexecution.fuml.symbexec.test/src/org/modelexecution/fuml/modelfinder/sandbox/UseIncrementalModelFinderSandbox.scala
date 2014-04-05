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
import org.tzi.use.kodkod.transform.enrich.ObjectDiagramModelEnricher
import org.tzi.use.uml.mm.ModelFactory
import org.tzi.use.uml.ocl.expr.Expression
import org.tzi.use.parser.ocl.OCLCompiler
import java.io.PrintWriter
import org.tzi.use.parser.Symtable
import org.tzi.use.parser.SrcPos

/*
 * Notes:
 * 
 * We have to add ID-attributes to every class to be able to directly select objects in
 * the invariants for expressing path conditions.  
 * 
 * Undefined attributes and links will be set by kodkod. So we should keep track during
 * the symbolic execution which of those have actually been necessary for the execution
 * and retain only those. Otherwise, we may not get a result caused by a violation of a
 * new path condition that is invalidated by an uninteresting attribute or link. 
 */

object UseIncrementalModelFinderSandbox extends App {
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
      "Lecture", "lectures", "0..*", MAggregationKind.NONE,
      "Person", "person", "1", MAggregationKind.NONE)

    val system = UseSystemApi.create(model)
    val tanja = system.createObject("Person", "tanja")
    system.setAttributeValue("tanja", "name", "'Tanja'")
    system.setAttributeValue("tanja", "ects", "50")
    system.createObject("Person", "philip")
    system.setAttributeValue("philip", "name", "'Philip'")
    
    system.getSystem().getVariableEnvironment().assign("tanja", tanja.value())
    //system.getSystem().varBindings().push("tanja", tanja.value())
    
    //////////////////////////
    // Creating class invariants with variables
    val modelFactory = new ModelFactory()
    val personClass = model.getClass("Person")
    val invBody = "self = tanja implies self.lectures->exists(l | self.ects > l.ects and self.name <> l.name)"
    val vars = new Symtable();
    vars.add("self", personClass.`type`() , new SrcPos("self", 1, 1));
    vars.add("tanja", personClass.`type`() , new SrcPos("tanja", 1, 1));
    val invExp = OCLCompiler.compileExpression(model, invBody, "UseApi", new PrintWriter(System.err), vars);
    val classInv = modelFactory.createClassInvariant("TestInv", Set("tanja").toList, personClass, invExp, false)
    model.addClassInvariant(classInv)
    //////////////////////////
    
    //api.createInvariant("TestInv", "Person", "self.name = 'Tanja' implies self.lectures->exists(l | self.ects < l.ects and self.name <> l.name)", false)
    api.createInvariant("TestInv2", "Person", "let n:String = 'Test' in self.name = 'Philip' implies self.lectures->one(l | self.name = l.name)", false)
    api.createInvariant("TestInv3", "Person", "(self.name = 'Tanja' or self.name = 'Philip') and Person.allInstances()->forAll(a | a <> self implies self.name <> a.name)", false)
    //api.createInvariant("TestInv", "Lecture", "self.ects > 0 and self.ects < self.person.ects", false)

    // println(system.evaluate("Person.allInstances()->select(name = 'Tanja')"))

    tryGenerator(system)
  }

  def tryGenerator(systemApi: UseSystemApi) {
    val system = systemApi.getSystem()
    val model = system.model
    val iModel = PluginModelFactory.INSTANCE.getModel(model)

    val defaultConfigFile = new File("test.properties")
    //    val defaultConfigVisitor = new DefaultConfigurationVisitor("test.use")
    //    val defaultConfigFile = defaultConfigVisitor.getFile()
    //    iModel.accept(defaultConfigVisitor)

    val propertyConfigurator = new PropertyConfigurationVisitor(defaultConfigFile.getAbsolutePath())
    iModel.accept(propertyConfigurator);

    enrichModelWithLoadedInvariants(system, iModel)
    new ObjectDiagramModelEnricher().enrichModel(system, iModel)

    new UseKodkodModelValidator(system).validate(iModel)

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
    val configurator = model.getConfigurator().asInstanceOf[ModelConfigurator]
    configurator.clear()
    val invariantTransformator = new InvariantTransformator(model.modelFactory(), model.typeFactory())
    

    for (inv <- mSystem.model().classInvariants()) {
      val transformedInv = invariantTransformator.transform(model, inv)
      configurator.addInvariant(transformedInv)
    }
  }

}