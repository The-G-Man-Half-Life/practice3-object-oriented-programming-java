# Practice Number 3 - Object Oriented Programming in Java

This project is focused on developing multiple linear regression models to predict data with a high level of certainty by implementing some basic functionalities such as: "fit"(Train the model), predict(obtain predictions), score(computes prediction errors) and data scaling(scaling the data). All this for obtaining information to compare the results with some real data and therefore calculate information such as: "weights"(indicates how much influence has the data on the results) and "bias"(indicates the sistematic error that the model has when comparing the obtained data with the real data).

## Contents
- `data`
  - `IceCreamSellingData.csv`
  - `StudentExamScores.csv`
- `docs`
  - `README.md`
  - `VideoLink.txt`
- `output`
  - `predictionsMultiple.txt`
  - `predictionsSimple.txt`
- `src`
  - `models`
    - `LinearRegression.java`
  - `tests`
    - `LinearRegressionTest.java`
  - `utils`
    - `CVSReader.java`
    - `MatrixOperations.java`
  - `Main.java`
- `.gitignore`

## Program Functionalities

### Utils

- **CVSReader.java**  
  It takes care of gathering all of the informations inside
  IceCreamSellingData.cvs and StudentExamScores.cvs in an array of doubles
  and to do this it uses two secondary functions:
  - *parseRows*  
  Takes care of receiving the file path and the matrix in which the
  information will be gathered and then by putting the information of a
  data in a double list it puts it in the matrix as a row, so it does
  every single data.
  - *read*  
  Takes care of creating the matrix then casting parseRows to do what
  was said before and finally returns the matrix with all the info.

- **MatrixOperations.java**  
  It takes care of containing all the required matrix functions to develop
  the multiple linear regression model such as:
  - *multiplyMatrix*  
  Takes care of verifying that the matrixes about to be multiplied are
  able to do so, and after the verification if they are incompatible the
  function will return an error message in the opposite case it will return
  the multiplied matrix.
  - *transpose*  
  Takes care of transposing the given matrix by turning the rows into columns.
  After that it will return the transposed matrix.
  - *inverse*  
  It generates the inverse of the matrix, suppossing it has a determinant
  distinct from 0, and to do it so, it makes an augmented matrix with the
  identity matrix and after that it uses the Gauss-Jordan method to obtain
  its inverse.
  - *jointMatrixHorizontal*  
  Takes care of joining two matrixes into one, having in mind that these
  have the same amount of rows, making the new matrix one with the common
  amount of rows but with the sum of the total amount of columns of the
  two originals.
  - *splitMatrixHorizontalLeft*  
  Takes care of dividing the matrix into two of the same size but only
  returns the part in the left.
  - *splitMatrixHorizontalRight*  
  Takes care of dividing the matrix into two of the same size but only
  returns the part in the right.
  - *splitMatrixVerticalTop*  
  Takes care of dividing the matrix into two of the same size but only
  returns the part in the top.
  - *splitMatrixVerticalBottom*  
  Takes care of dividing the matrix into two of the same size but only
  returns the part in the bottom.
  - *toColumnMatrix*  
  Takes care of turning rows into columns.
  - *gaussJordan*  
  It replicates the gauss jordan method to obtain the reduced stepped matrix.
  - *splitMatrixVerticalTop*  
  Takes care of dividing the matrix into two of the same size but only
  returns the part in the top.
  - *subtractVectors*  
  Takes care of subtracting one vector with another one and finally it returns
  the subtracted vector.
  - *multiplyVector*  
  Takes care of multiplying a vector with a scalar from the Real numbers and then
  returns the multiplied vector.
  - *divideVector*  
  Takes care of dividing a vector by a scalar from the Real numbers and then returns
  the simplified vector.
  - *swappingRow*  
  Swapes two rows of one matrix given.
  - *getIdentityMatrix*  
  Returns a square identity matrix according to the requested size.
  - *soutMatrix*  
  Method implemented to print the matrixes in a visually understandable way.
  - *soutVector*  
  Prints the vector of a given array in a a visually understandable way.

### Models

- **LinearRegression.java**  
  This is the main model that takes care of handling the attributes behind the linear
  regression model such as the weight and the bias needed for the model but also
  the means and the stds required for the data scaling process. Nonetheless it also
  contains the methods to make the linear regression model work such as:
  - *fit*  
  Takes care of training the model by implementing the normal equation but for that
  it casts some other methods from the MatrixOperations.java file the normal equation
  process occurs in this way: It scales the data, then to obtain the bias ,it transposes
  the matrix, multiplies the transposed matrix with the original one, obtains the inverse
  matrix obtained from the last process, then multiplies this matrix with the data that
  is wished to predict the data from and finally it extracts the bias  and the weight after
  that this process occurs. This proccess occurs using these next  methods respectively:
  dataScaling, addBiasColumn, MatrixOperations.transpose, MatrixOperations.multiplyMatrix,
  MatrixOperations.inverse, MatrixOperations.multiplyMatrix, MatrixOperations.multiplyMatrix
  and lastly getColumn. This method does this entire process in a try catch method to notify
  possible errors.
  - *dataScaling*  
  By using a process known as standardization it calculates the means and standard
  deviations(stds) so in that way it can scale the data to some more reasonable values that
  the model can work with. Eviting some big scale weight problems due to the inmensity of
  some values.
  - *scaleRow*  
  Scales a single row by using the means and the standard deviation obtained from the training.
  It remains there if needed for scaling the project.
  - *predict(double[] x)*  
  This metod takes care of predicting the results of the given data by using the weight
  and the bias obtained previously.
  - *predict(double[][] x)*  
  This method takes care of returning the predicted values by using the method before and
  it stores them in a list of doubles.
  - *score*  
  Calculates how well the model fits the data using the R^2 score formula:
  R^2 = 1 - (sum of squared errors / total sum of squares).
  - *addBiasColumn*  
  Takes care of adding a column full of ones that can be used with the original matrix so
  after the normal equation is done it can return the bias.
  - *mean*  
  Takes care of calculating the average of the true values of Y.

### Tests

- **LinearRegressionTest.java**  
  This is the model that takes care of gathering all the methods and required data to
  execute the linear regression model
  - *LinearRegressionTest*  
  Takes care of gathering everything from the other models such as values, methods,
  files etc... So output test can print the results.
  - *outputTest*
  This method takes care of printing all the results obtained from the test.

## Program Extras

### Data
- **IceCreamSellingData.csv**  
  Contains all the ice cream selling data in a scv format.
- **StudentExamScores.csv**  
  Contains all the student exam scores data in a scv format.
  
### Docs
- **Readme.md**  
  Contains this readme.md file
- **VideoLink.txt**  
  Contains the link to the video.

### Output
- **predictionsMultiple.txt**  
  Contains the results after the predictions of the multiple linear regression
  model implemented with the StudenExamScores.csv data.
- **predictionsSimple.txt**  
  Contains the results after the predicion of the simple linear regression model
  implemented with the IceCreamSellingData.csv data.

### Main.java:
  The main file that takes care of executing the tests and the entire program.

#### Examples of the outputs of the multiple linear regression test:

```Java
Multiple linear regression test:

Test for data in "..\data\StudentExamScores.csv" (scaled)
Weights:        0.7392711028529284
        0.2099788845164484
        0.2274887497442752
        0.40900593694931975
Bias: 8.325947582256633E-16
Score: 0.8414239969362044
```

#### Examples of the outputs of the simple linear regression test:

```Java
Simple linear regression test:

Test for data in "..\data\IceCreamSellingData.csv" (scaled)
Weights:        -0.1751842927078433
Bias: 1.7748982594039712E-16
Score: 0.03068953641154748
```

## Problems and errors during the development

### Utils
- **CVSReader.java**  
  - Understanding how to make the program to be able to gather
    the data that is in distinct format styles but similar parsing
    of the data
  -  Making the functions be able to work together without inconvenients
  -  Adjusting the logic for saving the information in a proper way
- **MatrixOperations.java**  
  - Understanding how to manage the data in rows and lists
  - Understanding how to make all the functions as general as possible
    for all the possible scenarios
  - Creating the specialized methods for specific functions like gauss-
    jordan
  - Having to change from floats to doubles to reduce the error margin that
    could happen
  - Taking into account all the possible scenarios in which the functions
    can not work properly to predict them and prevent them.
  - Understanding how to integrate the extra columns to an original column
  - Keeping the logic simple as can ocurr a lot of confusions with the rows

### Models
- **LinearRegression.java**  
  - Planning how to set up everything properly in the fit as it is the main
    method that almost all the other ones rely in.
  - Naming the variables simple but short enough to not oversaturate the method
    with letters and words.
  - Diferentiating how big was the impact of the data scaling function inside
    the project to understand if it was good implemented or not.
  - With the predict methods was finding how to separate properly the functiona-
    lities to maintain the clean code bases and also deciding how to diferentiate
    them.
  - In score finding a good way to proccess the information that it would receive
    from the obtained results and the real results.
  - Where to place the addBiasColumn method as it wasnt necessarily a method from
    operation matrix strictly but also because it was not so fitting in this file
    so it was decided to leave it in this file for convenience.
  - Deciding the kind of access modifiers that the functions would have as although
    some methods were important we didnt considere them important enough to implement
    them the getters setters logic which involves more time accessing and processing.
    
### Tests
- **LinearRegressionTest.java**  
  -Finding a way to cohesively joining all the functions and methods created before to
   execute the program and test if everything was made correctly.
  -Separating the functionality of the test with the printing of the information to
   organize properly the model.
  -making the printing of the information visually appealing due to all the information it
  contained 

### Data
- **IceCreamSellingData.csv**  
  -Deciding wheter or not we should copy all the information into a matrix to simplify the
  process or we should make the logic to convert all the information of both csv to turn
  them into matrixes so we decided this one to learn more.
- **StudentExamScores.csv**  
  -After doing the lineal regression model of this data we found out this one had quite
  some strange behaviors with the data as the model returned some strange weights and bias
  so we found out that it hada a quadratic behavior which was the reason behin all that.
### Docs
- **Readme.md**  
  -Organizing the document to be easily readable and understood.
  
### Output
- **predictionsMultiple.txt**  
 - There were no inconvenients just placing the outputs to rewatch them if needed.
- **predictionsSimple.txt**  
  - There were no inconvenients.

### Main.java:
  - Printing the information in a good way.
  
### General problems throught the development

   - Installing the jdk and all of the nece-
     ssary components in VSC to make everything
     run smoothly.
   - Understanding how to separate all the project
     in a clean way to maintain a good workflow
     and keep everything separated and easily
     accesable.
   - Coordinating all the proccess through Github.
     Since we divided the work between all members
     coordinating how to use the tool appropiately
     to evite fatal mistakes was really important
     and also to inform the proper structures for
     the documentation, the commentaries and the
     kind of typing.
   - Time organization and video preparation.
   - Understanding how to use the modules to import
     and export codes between files so in that way the
     code could be more readable and better to maintain
     along the whole process.
   - Implementing the matrixes in java as we didnt
     have access to libraries which involved studying
     again linear algebra for this entire process.
   - How to separate the branch organization as this
     project involved a lot of of interoperativity
     so we needed to make the branches share information
     quicly without losing the separation needed for
     a clear workflow.
     
## Conclusions

  - The development of this project taught us the im-
    portance of separating the requirements properly
    between all the memebers to agilize the development.
  - It is really important to understand how to sepa
    rate properly all the functionalities and programs
    inside the project to maintain a good cohesion and
    order along the whole development proccess,
    so the code can be understood by the others that
    are working on different things.
  - It is necessary to understand the use of tools
    such as Github and VSC to collaborate with other
    people to bring to life all kind of different projects
    one could ever have specially the usage of branches
    and merging as these can save the project from big
    mistakes but also can destroy the entire project.
  - Understanding how to make things from scratch
    can be extremely useful so once you start implementing
    libraries and other people's code you know how to
    deal with certain problems and the functionality
    behind it.
  - The teamwork was really important in this project
    as We needed feedback from each other as the project
    required from everyone part of the work.
  - Good expression and communication were vital elements
    to make a good video.
    
## tests (Inside the project you can find these inside the output folder)

```bash
Multiple linear regression test:

Test for data in "..\data\StudentExamScores.csv" (scaled)
Weights:        0.7392711028529284
        0.2099788845164484
        0.2274887497442752
        0.40900593694931975
Bias: 8.325947582256633E-16
Score: 0.8414239969362044


Simple linear regression test:

Test for data in "..\data\IceCreamSellingData.csv" (scaled)
Weights:        -0.1751842927078433
Bias: 1.7748982594039712E-16
Score: 0.03068953641154748
```

## Prerequisites to execute this repository
- Install the Java Development Kit(JDK) from --> https://www.java.com/en/download/manual.jsp
- Visual Studio Code. https://code.visualstudio.com/download (or your prefered IDE java)
- Have installed git from --> https://git-scm.com/install/

## Where to Use
- It can be used in VSC, netBeans, InteliJ or your IDE  of trust. 

## How to Use
- Watch the bash template to see how to execute every query.

```bash
# 1) Clone and enter the repo
  git clone https://github.com/The-G-Man-Half-Life/practice3-object-oriented-programming-java.git
  cd practice3-object-oriented-programming-java

# 2) access to the src folder where the main file resides
  cd src

# 3) run the program
  java Main.java

// if wished to review the results in a more comprehensive way pls look at the ooutput folder
where are the console outputs
  
```
Made by: Mateo Montoya Ospina and Juan Pablo Lopez Lidueña










