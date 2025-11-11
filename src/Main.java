import tests.LinearRegressionTest;

public class Main {
	public static void main(String[] args) {
		// Casting the tests in Main.java to execute them and see the results

		// Multiple linear regression test
		System.out.println("Multiple linear regression test:");
		LinearRegressionTest test1 = new LinearRegressionTest("..\\data\\StudentExamScores.csv");
		test1.outputTest();


		// Simple linear regression test
		System.out.println("Simple linear regression test:");
		LinearRegressionTest test2 = new LinearRegressionTest("..\\data\\IceCreamSellingData.csv");
		test2.outputTest();
	}
}