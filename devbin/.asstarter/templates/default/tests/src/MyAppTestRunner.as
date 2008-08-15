import flexunit.framework.TestSuite;

private function onCreationComplete():void
{
    testRunner.test = createSuite();
    testRunner.startTest();
}

private function createSuite():TestSuite
{
    var ts:TestSuite = new TestSuite();

    //ts.addTestSuite();
    return ts;
}
