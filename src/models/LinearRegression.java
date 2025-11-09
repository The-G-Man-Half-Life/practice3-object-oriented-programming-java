package models;
import utils.MatrixOperations;
public class LinearRegression {
	private double[] weights;
	private double bias;


	public void fit(double[][] x, double[] y) {
		// With the Normal Equation W = (X^t X)^-1 X^t y
		try {
			// To get the bias we add a column of 1 and procede with the rest.
			double[][] xB = addBiasColumn(x);

			// Get the transposed of X (X^t)
			double[][] xT = MatrixOperations.transpose(xB);

			// Get multiplication of X^t by X
			double [][] xTx = MatrixOperations.multiplyMatrix(xT, xB);

			// Get the inverse of (X^t X)
			double[][] xTxInv = MatrixOperations.inverse(xTx);

			// Get the multiplication of X^t by Y
			double[][] xTy = MatrixOperations.multiplyMatrix(xT, MatrixOperations.toColumnMatrix(y));

			double[][] column = MatrixOperations.multiplyMatrix(xTxInv, xTy);

			// Extract the Bias
			bias = column[0][0];
			double[][] wB = MatrixOperations.splitMatrixVerticallBotton(column, 1);

			// Extract the Weights
			weights = MatrixOperations.getColumn(wB, 0);
		}
		catch (Exception e) {
			System.out.println(e.getMessage());
		}
	}

	// By itself the normal function doesn't give the bias, but we can add another column full of 1 and after the normal equation it should give us the bias.
	private static double[][] addBiasColumn(double[][] x) {
		int n = x.length;

		double[][] biasColumn = new double[n][1];
		for (int i = 0; i < n; i++) {
			biasColumn[i][0] = 1;
		}
		
		double[][] xB = MatrixOperations.jointMatrixHorizontal(biasColumn, x);
		return xB;
	}


	public double[] getWeights() {
		return weights;
	}

	public double getBias() {
		return bias;
	}
}