import tests.LinearRegressionTest;

public class Main {
	public static void main(String[] args) {
		LinearRegressionTest test1 = new LinearRegressionTest("..\\data\\StudentExamScores.csv");
		test1.outputTest();

		LinearRegressionTest test2 = new LinearRegressionTest("..\\data\\IceCreamSellingData.csv");
		test2.outputTest();
	}
}