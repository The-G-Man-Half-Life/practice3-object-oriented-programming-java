package models;
import utils.MatrixOperations;

public class LinearRegression {
	private double[] weights;
	private double bias;

	// For Data Scallig
	private double[] means;
	private double[] stds;



	/*		METHODS		*/
	public void fit(double[][] x, double[] y) {
		// With the Normal Equation W = (X^t X)^-1 X^t y
		try {
			// To get the BIAS we add a column of 1 and procede with the rest.
			double[][] xBias = addBiasColumn(x);

			// Get the transposed of X (X^t)
			double[][] xT = MatrixOperations.transpose(xBias);

			// Get multiplication of X^t by X
			double [][] xTx = MatrixOperations.multiplyMatrix(xT, xBias);

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



	/**
	 * Scales the given matrix X using standardization (Z-score normalization).
	 * It also stores the means and stds so future data can be scaled consistently.
	 * @param x Input matrix to scale.
	 * @return Scaled matrix.
	 */
	public double[][] dataScaling(double[][] x) {
		int n = x.length;
        int m = x[0].length;
        means = new double[m];
        stds = new double[m];

        // Calculate Means
        for (int j = 0; j < m; j++) {
            double sum = 0;
            for (int i = 0; i < n; i++) sum += x[i][j];
            means[j] = sum / n;
        }

        // Calculate std deviations (population).
        for (int j = 0; j < m; j++) {
            double sumSq = 0;
            for (int i = 0; i < n; i++) {
                double diff = x[i][j] - means[j];
                sumSq += diff * diff;
            }
            stds[j] = Math.sqrt(sumSq / n);
            if (stds[j] == 0) stds[j] = 1; // evitar division by zero
        }

		// Apply Scaling
        double[][] xScaled = new double[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                xScaled[i][j] = (x[i][j] - means[j]) / stds[j];
            }
        }
        return xScaled;
	}



	/**
	 * Scales a single sample (row) using the same means and stds from training.
	 * @param x Input row.
	 * @return Scaled row.
	 */
	public double[] scaleRow(double[] x) {
		int m = x.length;
        double[] row = new double[m];
        for (int j = 0; j < m; j++)
			row[j] = (x[j] - means[j]) / stds[j];
        return row;
	}


 
	/**
	 * This metod takes the weights, bias and new values and gives the predicted result.
	 * @param x Abscissas.
	 * @return Prediction (Ordinate).
	 */
	public double predict(double[] x) {
		int n = x.length;
		double yPred = bias;

		// New Values and the Weights need to have same lenght.
		if (n != weights.length) return 0;

		for (int i = 0; i < n; i++)
			yPred += x[i] * weights[i];

		return yPred;
	}


 
	/**
	 * This metod takes the weights, bias and new values and gives the predicted result.
	 * @param x Abscissas.
	 * @return Prediction (Ordinate).
	 */
	public double[] predict(double[][] x) {
		int n = x.length;

		double[] yPred = new double[n];
		
		for (int i = 0; i < n; i++)
			yPred[i] = predict(x[i]);

		return yPred;
	}


 
	/**
	 * Calculates how well the model fits the data using the R^2 score.
	 * R^2 = 1 - (sum of squared errors / total sum of squares).
	 * @param x Input samples.
 	 * @param y Real (expected) values.
	 * @return R^2 score, where 1 means perfect fit and 0 or less means bad fit.
	 */
	public double score(double[][] x, double[] y) {
		double[] yPred = predict(x);

		double meanY = mean(y);

		double sumErr = 0;
		double sumTotal = 0;

		for (int i = 0; i < y.length; i++) {
			sumErr += Math.pow(y[i] - yPred[i], 2);
			sumTotal += Math.pow(y[i] - meanY, 2);
		}

		// Shouldn't we sqrt the ressult because its R^2?
		return 1 - (sumErr / sumTotal);
	}



	/*		HELPER FUNCTIONS		*/
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



	/**
	 * This method only gets the mean (Average) of the true values of Y.
	 * @param y Array of values.
	 * @return The mean (Average) value.
	 */
	private double mean(double[] y) {
		double meanY = 0;
		for (double value : y) meanY += value;
		meanY /= y.length;
		return meanY;
	}



	//      GETTERS
	public double[] getWeights() {return weights;}
	public double getBias() {return bias;}
	public double[] getMeans() { return means;}
    public double[] getStds() { return stds; }
}