package utils;

public abstract class MatrixOperations {
	/**
	 * This method multiplies two Matrix, Anxm and Bmxp.
	 * @param matrixA
	 * @param matrixB
	 * @return The multiplication of AxB
	 * @throws Exception When A and B don't have the right size.
	 */
	public static double[][] multiplyMatrix(double[][] matrixA, double[][] matrixB) throws Exception {
		int n = matrixA.length;
		int m = matrixA[0].length;
		int p = matrixB[0].length;

		if (m != matrixB.length) throw new Exception("Matrix A and B don't have right size.");

		double[][] matrixC = new double[n][p];

		for (int i = 0; i < n; i++)
			for (int j = 0; j < p; j++) {
				matrixC[i][j] = 0;
				for (int k = 0; k < m; k++)
					matrixC[i][j] += matrixA[i][k] * matrixB[k][j];
			}

		return matrixC;
	}
	
	
	
	/**
	 * This method takes any Matrix X NxM and returns its Transposed.
	 * @param matrix X
	 * @return Transposed of X (X^T).
	 */
	public static double[][] transpose(double[][] matrix){
		// Size of the matrix
		int n = matrix.length;
		int m = matrix[0].length;

		// Empty Matrix size MxN
		double[][] transposedMatrix = new double[m][n];

		for (int i = 0; i < m; i++) {
			for (int j = 0; j < n; j++) {
				transposedMatrix[i][j] = matrix[j][i];
			}
		}

		return transposedMatrix;
	}


	
	//Because we are multiplying the Matrix with its Transposed, we are getting a square matrix.
	/**
	 * This method take any Matrix X and we find its inverse, through the Gauss-Jordan elimination.
	 * The Matrix must be square.
	 * @param matrix X
	 * @return Inverse of X (X^-1)
	 */
	public static double[][] inverse(double[][] matrix) {
		int n = matrix.length;

		// We generate the aumented matrix.
		double[][] identityMatrix = getIdentityMatrix(n);
		double[][] augmentedMatrix = jointMatrixHorizontal(matrix, identityMatrix);
		
		// Do Gauss-Jordan
		augmentedMatrix = gaussJordan(augmentedMatrix);

		// Split array to get inverse
		double[][]inverseMatrix = splitMatrixHorizontalRight(augmentedMatrix, n);

		return inverseMatrix;
	}



	/**
	 * This method joins two matrix into one. The two matrix to join should have same number N of rows.
	 * @param matrixA Matrix at the right.
	 * @param matrixB Matrix at the left.
	 * @return
	 */
	public static double[][] jointMatrixHorizontal(double[][] matrixA, double[][] matrixB ) {
		int n = matrixA.length;
		int mA = matrixA[0].length; int mB = matrixB[0].length;

		double[][]jointMatrix = new double[n][mA + mB];

		for (int i = 0; i < n; i++) {
			// Input a row from Matrix A
			for (int j = 0; j < mA; j++)
				jointMatrix[i][j] = matrixA[i][j];

			// Input a row from Matrix B
			for (int j = 0; j < mB; j++)
				jointMatrix[i][j + mA] = matrixB[i][j];
		}
		return jointMatrix;
	}



	/**
	 * This function divides the matrix into two and returns the first part.
	 * @param matrix Matrix to split.
	 * @param column Column(Index) from which the second part is discarded.
	 * @return First part of the input matrix.
	 */
	public static double[][] splitMatrixHorizontalLeft(double[][] matrix, int column) {
		int n = matrix.length;
		int m = column;

		double[][] splitMatrix = new double[n][m];

		// We copy the given values.
		for (int i = 0; i < n; i++)
			for (int j = 0; j < m; j++)
				splitMatrix[i][j] = matrix[i][j];

		return splitMatrix;
	}



	/**
	 * This function divides the matrix into two and returns the second part.
	 * @param matrix Matrix to split.
	 * @param column Column(Index) up until the first part is discarded.
	 * @return Second part of the input matrix.
	 */
	public static double[][] splitMatrixHorizontalRight(double[][] matrix, int column) {
		int n = matrix.length;
		int m = matrix[0].length - column;

		double[][] splitMatrix = new double[n][m];

		// We copy the given values.
		for (int i = 0; i < n; i++)
			for (int j = 0; j < m; j++)
				splitMatrix[i][j] = matrix[i][column + j];

		return splitMatrix;
	}



	/**
	 * This method returns a column of a Matrix as a single vector (array).
	 * @param matrix {@code double[][]}
	 * @param column Integer index of Columnto be extracted.
	 * @return {@code double[]} Columnwith index {@code columnIndex}
	 */
	public static double[] getColumn(double[][] matrix, int columnIndex) {
		int n = matrix.length;
		double[] column = new double[n];

		for (int i = 0; i < n; i++)
			column[i] = matrix[i][columnIndex];

		return column;
	}



	/**
	 * This method find the Reduced Row Echelon Form of a  not singular Matrix
	 * @param matrix {@code double[][]} Matrix to be reduced.
	 * @return The Reduced Row Echelon From of the input matrix.
	 */
	public static double[][] gaussJordan(double[][] matrix) {
		int n = matrix.length;
		
		// We must loop for each Pivot
		for (int pivot = 0; pivot < n; pivot++) {
			// We look for the largest number in the row.
			int row = pivot;
			for (int i = pivot; i < n; i++) {
				if (Math.abs(matrix[i][pivot]) > Math.abs(matrix[row][pivot]))
					row = i;
			}
			
			// Then we swap the row with the largest value.
			matrix = swappingRow(matrix, pivot, row);
			
			// We must Normalize the PIVOT with its ROW.
			double pivotValue = matrix[pivot][pivot];
			divideVector(matrix[pivot], pivotValue);

			// Finally we Eliminate other rows.
			for (int i = 0; i < n; i++)
				if (i != pivot) {
					double factor = matrix[i][pivot];
					matrix[i] = subtractVectors(matrix[i], matrix[pivot], factor);
				}
		}

		return matrix;
	}



	/*		Row/Vector Operations for Gauss-jordan		*/



	/**
	 * This method subtracts any two vectors (Arrays) of the same dimention.
	 * @param vectorA Vector subtracted.
	 * @param vectorB Vector subtractor.
	 * @param scalar double value which multiplies the subtractor.
	 * @return The vector resulting from A - Scalar(B).
	 */
	public static double[] subtractVectors(double[] vectorA, double[] vectorB, double scalar) {
		// Vectors should be same size to subtract.
		if (vectorA.length != vectorB.length) return vectorA;

		int n = vectorA.length;
		double[] c = new double[n];
		for (int i = 0; i < n; i++)
			c[i] = vectorA[i] - (scalar * vectorB[i]);
		return c;
	}



	/**
	 * This method multiplies any vector with a scalar (real).
	 * @param vector To be multiplied.
	 * @param scalar To multiply.
	 * @return Vector multiplied.
	 */
	public static double[] multiplyVector(double[] vector, double scalar) {
		for (int i = 0; i < vector.length; i++)
			vector[i] *= scalar;
		return vector;
	}



	// This method divides any vector by a scalar (real).
	public static double[] divideVector(double[] v, double n) {
		if (n == 0) return v;
		for (int i = 0; i < v.length; i++)
			v[i] /= n;
		return v;
	}



	/**
	 * Method for swapping two rows in any matrix.
	 * @param matrix Matrix in which we are swapping rows.
	 * @param r1 First row to be swapped.
	 * @param r2 Second row to be swapped.
	 * @return Matrix with rows swapped.
	 */
	public static double[][] swappingRow(double[][] matrix, int r1, int r2) {
		// If same row is requested to swap or row is non-existent, we just return.
		int n = matrix.length;
		if (r1 == r2 || r1 >= n || r2 >= n) return matrix;

		// If we are given a non-existent row.

		double[] t = matrix[r1];
		matrix[r1] = matrix[r2];
		matrix[r2] = t;
		return matrix;
	}



	/**
	 * Returns a Identity Matrix NxN, given any N value provided.
	 * @param n Size of matrix.
	 * @return Itentity matrix NxN.
	 */
	public static double[][] getIdentityMatrix(int n) {
		double[][] id = new double[n][n];
		for (int i = 0; i < n; i++)
			for (int j = 0; j < n; j++)
				id[i][j] = (i == j ? 1 : 0);
		return id;
	}



	// Temporal
	public static void soutMatrix(double[][] matrix) {
		for(double[] row : matrix) {
			for (double n : row)
				System.out.print("\t" + n);
			System.out.println();
		}
	}
}