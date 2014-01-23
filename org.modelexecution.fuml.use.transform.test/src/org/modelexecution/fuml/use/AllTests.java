package org.modelexecution.fuml.use;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.modelexecution.fuml.use.modelfinder.UseModelFinderTest;
import org.modelexecution.fuml.use.transform.FumlModel2UseModelTest;
import org.modelexecution.fuml.use.transform.FumlValues2UseValuesRoundtripTest;
import org.modelexecution.fuml.use.transform.FumlValues2UseValuesTest;

@RunWith(Suite.class)
@SuiteClasses({ UseModelFinderTest.class, FumlModel2UseModelTest.class,
		FumlValues2UseValuesTest.class, FumlValues2UseValuesRoundtripTest.class })
public class AllTests {

}
