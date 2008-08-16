import flexunit.framework.TestSuite;
import MyAppTest;

private function onCreationComplete():void
{
    testRunner.test = createSuite();
    testRunner.startTest();
}

private function createSuite():TestSuite
{
    var ts:TestSuite = new TestSuite();

    ts.addTestSuite(MyAppTest);
    return ts;
}
