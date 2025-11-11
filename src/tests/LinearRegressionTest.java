package tests;

import models.LinearRegression;
import utils.*;

public class LinearRegressionTest {
    private String dataFilePath;
    private double[][] dataX;
    private double[] dataY;
    private LinearRegression multipleLinearRegression = new LinearRegression();

    // method that takes care of bringing all necessary components for testing the model
    public LinearRegressionTest(String path) {
		// First we need to read the values from the CVS given.
		// We can stablish a new class that is only about this.
        this.dataFilePath = path;

		double[][] dataRaw = CVSReader.read(path);
		
		// With the data already inside a Matrix we must only divide the output, which just so happens to be the last column.
		int n = dataRaw[0].length;

		// Matrix of data.
		dataX = MatrixOperations.splitMatrixHorizontalLeft(dataRaw, n - 1);

		// Vector of results
		dataY = MatrixOperations.getColumn(dataRaw, n - 1);

		// Linear Regresion
		multipleLinearRegression.fit(dataX, dataY);
    }

	//method that takes care of printing all the obtained results
    public void outputTest() {
		// printing the results
        System.out.println("\nTest for data in \"" + dataFilePath + "\" (scaled)");
		System.out.print("Weights:");
		MatrixOperations.soutVector(multipleLinearRegression.getWeights());
		System.out.println("Bias: " + multipleLinearRegression.getBias());
		System.out.println("Score: " + multipleLinearRegression.score(dataX, dataY));
		System.out.println("\n");
    }
}