import java.util.ArrayList;
import java.util.Scanner;
import java.io.File;

public abstract class CVSReader {
	/**
	 * This method receives a file path, reads its data and parses it to a float Matrix.
	 * 
	 * @param file_Path The relative or absolute path for the CVS file containing data.
	 * @return Float matrix.
	 */
	public static float[][] read(String file_Path) {
		ArrayList<float[]> temp_matrix = new ArrayList<>();
		
		temp_matrix = parse_Rows(file_Path, temp_matrix);

		float[][] matrix = new float[temp_matrix.size()][];
		for (int i = 0; i < matrix.length; i++)
			matrix[i] = temp_matrix.get(i);

		return matrix;
	}

	private static ArrayList<float[]> parse_Rows(String file_Path, ArrayList<float[]> temp_matrix) {
		try {
			Scanner reader = new Scanner(new File(file_Path));

			// Because we need to skip the header
			reader.nextLine();

			while (reader.hasNextLine()) {
				String[] values = reader.nextLine().split(",");
				float[] parsed_values = new float[values.length];

				// Here we attempt to get the real value of each number.
				for (int i = 0; i < values.length; i++)
					parsed_values[i] = Float.parseFloat(values[i]);

				// We add the row into the matrix.
				temp_matrix.add(parsed_values);
			}
			reader.close();
		} catch (Exception e) {System.out.println(e.getMessage());}
		return temp_matrix;
	}
}