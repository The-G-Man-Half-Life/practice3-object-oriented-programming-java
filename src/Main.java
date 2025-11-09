import utils.CVSReader;
import utils.MatrixOperations;

public class Main {
	public static void main(String[] args) {
		// First we need to read the values from the CVS given.
		// We can stablish a new class that is only about this.

		double[][] data = CVSReader.read("..\\data\\StudentExamScores.csv");
		System.out.println("Rows: " + data.length);
		System.out.println("Colums: " + data[0].length);

		MatrixOperations.soutMatrix(data);
	}
}