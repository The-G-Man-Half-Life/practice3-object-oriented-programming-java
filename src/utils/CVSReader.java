package utils;
import java.util.ArrayList;
import java.util.Scanner;
import java.io.File;

public abstract class CVSReader {
	/**
	 * This method receives a path for a CVS file, parses it and returns a double matrix.
	 * @param filePath Relative or Absolute path for CVS.
	 * @return {@code double[][]} of CVS data.
	 */
	public static double[][] read(String filePath) {
		ArrayList<double[]> tempMatrix = new ArrayList<>();
		
		tempMatrix = parseRows(filePath, tempMatrix);

		double[][] matrix = new double[tempMatrix.size()][];
		for (int i = 0; i < matrix.length; i++)
			matrix[i] = tempMatrix.get(i);

		// Because what the teacher said but I still don't get it.
		return MatrixOperations.transpose(matrix);
	}

	private static ArrayList<double[]> parseRows(String file_Path, ArrayList<double[]> temp_matrix) {
		try {
			Scanner reader = new Scanner(new File(file_Path));

			// Because we need to skip the header
			reader.nextLine();

			while (reader.hasNextLine()) {
				String[] values = reader.nextLine().split(",");
				double[] parsed_values = new double[values.length];

				// Here we attempt to get the real value of each number.
				for (int i = 0; i < values.length; i++)
					parsed_values[i] = Double.parseDouble(values[i]);

				// We add the row into the matrix.
				temp_matrix.add(parsed_values);
			}
			reader.close();
		} catch (Exception e) {System.out.println(e.getMessage());}
		return temp_matrix;
	}
}