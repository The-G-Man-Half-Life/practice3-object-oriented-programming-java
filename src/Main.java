import models.LinearRegression;
import utils.CVSReader;
import utils.MatrixOperations;
import utils.StandardScaler;

public class Main {
	public static void main(String[] args) {
		test("..\\data\\StudentExamScores.csv");
		test("..\\data\\IceCreamSellingData.csv");
	}

	// Testing function.
	public static void test(String dataFilePath) {
		// First we need to read the values from the CVS given.
		// We can stablish a new class that is only about this.
		double[][] data = CVSReader.read(dataFilePath);
		
		// With the data already inside a Matrix we must only divide the output, which just so happens to be the last column.
		int n = data[0].length;
		// Matrix of data.
		double[][] x = MatrixOperations.splitMatrixHorizontalLeft(data, n - 1);

		// Vector of results
		double[] y = MatrixOperations.getColumn(data, n - 1);

		// (Standard Scaling)
		StandardScaler scaler = new StandardScaler();
		scaler.fit(x);
		double[][] xScaled = scaler.transform(x);

		// Linear Regresion (Trained with scaled data)
		LinearRegression mlr = new LinearRegression();
		mlr.fit(xScaled, y);

		System.out.println("\nTest for data in \"" + dataFilePath + "\" (scaled)");
		System.out.print("Weights:");
		MatrixOperations.soutVector(mlr.getWeights());
		System.out.println("Bias: " + mlr.getBias());
		System.out.println("Score: " + mlr.score(xScaled, y));
		System.out.println("\n");
	}
}