import models.LinearRegression;
import utils.CVSReader;
import utils.MatrixOperations;

public class Main {
	public static void main(String[] args) {
		// First we need to read the values from the CVS given.
		// We can stablish a new class that is only about this.
		double[][] dataStudent = CVSReader.read("..\\data\\StudentExamScores.csv");
		
		// With the data already in side a Matrix we must only divide the output, which just so happens to be the last column.

		int n1 = dataStudent[0].length;
		double[][] x1 = MatrixOperations.splitMatrixHorizontalLeft(dataStudent, n1 - 1);
		double[] y1 = MatrixOperations.getColumn(dataStudent, n1 - 1);

		LinearRegression mlr1 = new LinearRegression();
		mlr1.fit(x1, y1);

		System.out.println("Weights:");
		MatrixOperations.soutVector(mlr1.getWeights());

		System.out.println("\nBias: " + mlr1.getBias());

		
		// First we need to read the values from the CVS given.
		// We can stablish a new class that is only about this.
		double[][] dataIceCream = CVSReader.read("..\\data\\IceCreamSellingData.csv");
		
		// With the data already in side a Matrix we must only divide the output, which just so happens to be the last column.

		int n2 = dataIceCream[0].length;
		double[][] x2 = MatrixOperations.splitMatrixHorizontalLeft(dataIceCream, n2 - 1);
		double[] y2 = MatrixOperations.getColumn(dataIceCream, n2 - 1);

		LinearRegression mlr2 = new LinearRegression();
		mlr2.fit(x2, y2);

		System.out.println("Weights:");
		MatrixOperations.soutVector(mlr2.getWeights());

		System.out.println("\nBias: " + mlr2.getBias());
	}
}