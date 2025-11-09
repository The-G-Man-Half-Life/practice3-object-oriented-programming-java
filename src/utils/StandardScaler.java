package utils;

public class StandardScaler {
    private double[] means;
    private double[] stds;

    public void fit(double[][] X) {
        int n = X.length;
        int m = X[0].length;
        means = new double[m];
        stds = new double[m];

        // calcular medias
        for (int j = 0; j < m; j++) {
            double sum = 0;
            for (int i = 0; i < n; i++) sum += X[i][j];
            means[j] = sum / n;
        }

        // calcular desviaciones (poblacional)
        for (int j = 0; j < m; j++) {
            double sumSq = 0;
            for (int i = 0; i < n; i++) {
                double d = X[i][j] - means[j];
                sumSq += d * d;
            }
            stds[j] = Math.sqrt(sumSq / n);
            if (stds[j] == 0) stds[j] = 1; // evitar division by zero
        }
    }

    public double[][] transform(double[][] X) {
        int n = X.length;
        int m = X[0].length;
        double[][] out = new double[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                out[i][j] = (X[i][j] - means[j]) / stds[j];
            }
        }
        return out;
    }

    public double[] transformRow(double[] x) {
        int m = x.length;
        double[] out = new double[m];
        for (int j = 0; j < m; j++) out[j] = (x[j] - means[j]) / stds[j];
        return out;
    }

    public double[] getMeans() { return means; }
    public double[] getStds() { return stds; }
}
