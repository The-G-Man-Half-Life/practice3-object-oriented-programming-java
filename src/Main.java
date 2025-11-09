import utils.CVSReader;
import utils.MatrixOperations;

public class Main {
	public static void main(String[] args) {
		// First we need to read the values from the CVS given.
		// We can stablish a new class that is only about this.

		double[][] data = CVSReader.read("..\\data\\StudentExamScores.csv");
		System.out.println("Rows: " + data.length);
		System.out.println("Colums: " + data[0].length);

		double[][] a = {
			{1, 2},
			{3, 4}
		};

		double[][] b = {
			{2, 0},
			{1, 2}
		};

		try {
			double[][] c = MatrixOperations.multiplyMatrix(a, b);
			MatrixOperations.soutMatrix(c);
		} catch (Exception e) {System.out.println(e.getMessage());}

		double[][] aInv = MatrixOperations.inverse(a);
		MatrixOperations.soutMatrix(aInv);
	}
}