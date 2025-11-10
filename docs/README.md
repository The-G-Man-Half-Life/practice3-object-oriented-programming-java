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
  -  
- **MatrixOperations.java**  
  - 

### Models
- **LinearRegression.java**  
  -

### Tests
- **LinearRegressionTest.java**  
  -

### Data
- **IceCreamSellingData.csv**  
  -
- **StudentExamScores.csv**  
  
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
  -

### .gitignore  
  -
  
### General problems throught the development

   - Installing swish-Prolog and all of the nece-
     ssary components to make it work because
     Microsoft defender evited the installation of
     certain files that were obligatory for the
     proper function of Prolog.
   - Understanding how to use Prolog in VSC as
     Prolog has its own console to execute the
     queries but we wanted to make it run on VSC's
     console for proper testing and more
     compatibility with Github and also due to
     the extensions that VSC provides.
   - Coordinating all the proccess through Github.
     Since we divided the work between all members
     coordinating how to use the tool appropiately
     to evite fatal mistakes was really important
     and also to inform the proper structures for
     the documentation, the commentaries the kind
     of typing for the code and the folder structure.
   - Time organization and video preparation.
   - Understanding how to use the modules to import
     and export codes between files so in that way the
     code could be more readable and better to maintain
     along the whole process
   - Understanding better the recursion as the prolog
     kind of recursion is quite much different to the
     one of haskell or an average POO one
   - Understanding the new types of variables for the
     facts since prolog includes the basic types but
     also some new ones as Atoms but with different types
     of atoms between each other.
   - Understanding some new methods in Prolog such as
     setof(), sort(), member() and findall() as these
     methods had their unique ways of working and were
     necessary for making the queries.
     
## Conclusions

  - The development of this project taught us the im-
    portance of learning and adapting to new languages
    to sort all kind of problems and difficulties
    specially when this brings a new understanding of
    different kinds of programming paradigms.
  - It is really important to understand how to sepa
    rate properly all the functionalities and programs
    inside the project to maintain a good cohesion and
    order along the whole development proccess,
    making it better to read and change if is necessary.
  - It is necessary to understand the use of tools
    such as Github and VSC to collaborate with other
    people to bring to life all kind of different projects
    one could ever have.
  - Understanding Recursion can be pretty tedious and
    annoying at first glance but learning it can open
    a world of possibilities for iteration problems.
  - Learning how to install new languages can be seen
    as a challenge, specially prolog and its problem
    with Microsoft Defender, but is really important
    to deal with this kind of challenges for the future 
  - The teamwork was really important in this project
    as We helped each other and learned tons of things
    of each other.
  - Good expression and communication was vital to make a
    good video.
    
## tests 

```bash
Output of `amdAfter2021PrintList/0`
--------------------------------------------
Trademark | Name | Serial number | Year of acquisition
--------------------------------------------
('HP'|'Omen 16'|'5CDV9D4N'|2023)
--------------------------------------------
('HP'|'Omen 15'|'5CDU1E5P'|2022)
--------------------------------------------
('Lenovo'|'LOQ 15 Gen 9'|'LNVW6D4N'|2023)
--------------------------------------------
('Lenovo'|'IdeaPad Gaming 3'|'LNVU1E5P'|2022)
--------------------------------------------
('ASUS'|'ROG Zephyrus G16'|'ASY8B2L'|2025)
--------------------------------------------
('ASUS'|'ROG Flow X13'|'ASZ7C3M'|2024)
--------------------------------------------
('ASUS'|'TUF Gaming A15'|'AST6D4N'|2023)
--------------------------------------------
('ASUS'|'ROG Flow Z13'|'ASTAB01'|2025)
--------------------------------------------
('MSI'|'Crosshair 17 HX'|'MSIU1E5P'|2024)
--------------------------------------------
('Razer'|'Blade 16 OLED'|'RZRY8B2L'|2025)
--------------------------------------------
('Razer'|'Blade 14 Ryzen'|'RZRW6D4N'|2023)
--------------------------------------------
('Dell'|'Alienware m15 R7'|'DLT3F6Q'|2023)
--------------------------------------------
('Razer'|'Tomahawk ATX'|'RZRDC02'|2024)
--------------------------------------------

Output of `amountOfCPASUSPrintList/0`
--------------------------------------------
This is the amount of ASUS computing platforms: 7
--------------------------------------------
These are all of the  ASUS computing platforms: 
--------------------------------------------
Name | Serial number | Year of acquisition | Type of Computing platform
--------------------------------------------
('ROG Strix Scar 18'|'ASX9A1Q'|2025|laptop)
--------------------------------------------
('ROG Zephyrus G16'|'ASY8B2L'|2025|laptop)
--------------------------------------------
('ROG Flow X13'|'ASZ7C3M'|2024|laptop)
--------------------------------------------
('TUF Gaming A15'|'AST6D4N'|2023|laptop)
--------------------------------------------
('ROG Strix G15'|'ASU1E5P'|2022|laptop)
--------------------------------------------
('ROG Flow Z13'|'ASTAB01'|2025|tablet)
--------------------------------------------
('ROG Flow Z13'|'ASTAB02'|2023|tablet)
--------------------------------------------

Output of `cpHDBetweenPrintList/0`
--------------------------------------------
Trademark | Name | Serial number | Hard disk capacity in GB
--------------------------------------------
('Lenovo'|'Legion Tab Gen 3'|'LNTG8A1X'|256)
--------------------------------------------

Output of `laptopsRamHDPrintList/0`
--------------------------------------------
This is the amount of Computing Platforms with
more than 4 GB of Ram and less than 512 GB of 
Hard Disk: 1
--------------------------------------------
These are all of the found computing platforms: 
--------------------------------------------
Name | Serial number | Ram capacity in GB | Hard disk capacity in GB
--------------------------------------------
('Legion Tab Gen 3'|'LNTG8A1X'|12|256)
--------------------------------------------

Output of `tablet2PlusGBRamPrintList/0`
--------------------------------------------
Trademark | Name | Serial number | Ram capacity in GB
--------------------------------------------
('Lenovo'|'Legion Tab Gen 3'|'LNTG8A1X'|12)
--------------------------------------------
('ASUS'|'ROG Flow Z13'|'ASTAB01'|128)
--------------------------------------------
('ASUS'|'ROG Flow Z13'|'ASTAB02'|32)
--------------------------------------------
('RedMagic'|'Astra Gaming Tablet'|'RDMTAB01'|24)
--------------------------------------------
('RedMagic'|'NOVA Gaming Tablet'|'RDMTAB02'|16)
--------------------------------------------

Output of `bestComputingPlatformPrint/0`
--------------------------------------------
The most powerful computing platform after 2023 is the: 
Legion 9i Gen 10
TradeMark: Lenovo
Serial number: LNVX9A1Q
Ram Capacity in GB: 192
Amount of CPU Cores: 24
Hard Disk Capacity in GB: 8192
Type of Computing Platform: laptop
VRam Capacity in GB: 24
Total punctuation: 209.184
--------------------------------------------

Output of `allBrandsMostCommonPrintList/0`
--------------------------------------------
Trademark | (CPU manufacturer, GPU manufacturer) | Total amount of counts 
--------------------------------------------
('ASUS',(amd,nvidia),3)
--------------------------------------------
('Dell',(intel,nvidia),6)
--------------------------------------------
('HP',(intel,nvidia),6)
--------------------------------------------
('Lenovo',(intel,nvidia),4)
--------------------------------------------
('MSI',(intel,nvidia),9)
--------------------------------------------
('Razer',(intel,nvidia),3)
--------------------------------------------
('RedMagic',(qualcomm,qualcomm),2)
--------------------------------------------

Output of `ramVRamMostCommonComboPrint/0`
--------------------------------------------
The most common combination of Ram and VRam 
capacity in GB is respectively: 32 and 8 with: 7 apparitions between all the 
computing platforms.
--------------------------------------------

Output of `worstComputingPlatformPrint/0`
--------------------------------------------
The least powerful computing platform before 2023 is the: 
Blade Stealth 13
TradeMark: Razer
Serial number: RZRU1E5P
Ram Capacity in GB: 16
Amount of CPU Cores: 4
Hard Disk Capacity in GB: 512
Type of Computing Platform: laptop
VRam Capacity in GB: 0
Total punctuation: 19.872
--------------------------------------------

Output of `print_All(bogota, medellin)`
--------------------------------------------
bogota (avion, 8->9, 100.0 USD)--> medellin
---
Costo Total: 100
Tiempo: 1h
--------------------------------------------
bogota (bus, 6->16, 40.0 USD)--> medellin
---
Costo Total: 40
Tiempo: 10h
--------------------------------------------

Output of `print_All(bogota, medellin, 6, 12)`
--------------------------------------------
bogota (avion, 8->9, 100.0 USD)--> medellin
---
Costo Total: 100
Tiempo: 1h
--------------------------------------------

Output of `print_Cheapest(bogota, cartagena)`
--------------------------------------------
bogota (bus, 6->16, 40.0 USD)--> medellin
medellin (bus, 7->19, 42.0 USD)--> cartagena
---
Costo Total: 82
Tiempo: 22h
--------------------------------------------

Output of `print_Fastest(bogota, cartagena)`
--------------------------------------------
bogota (avion, 8->9, 100.0 USD)--> medellin
medellin (bus, 7->19, 42.0 USD)--> cartagena
---
Costo Total: 142
Tiempo: 13h
--------------------------------------------
```

## Prerequisites to execute this repository
- SWI‑Prolog (recommended >= 8.0). https://www.swi-prolog.org/Download.html
- Visual Studio Code. https://code.visualstudio.com/download
- Have stablished and set up everything for Prolog

## Where to Use
- It can be used in the Prolog own terminal or preferably use the
powershell or VSC code terminal for better watch of code and execution.

## How to Use
- Watch the bash template to see how to execute every query.

```bash
# 1) Clone and enter the repo
  git clone The-G-Man-Half-Life/practice2-logical-programming-prolog
  cd practice2-logical-programming-prolog

# 2) enter challenge 1 database queries
  cd challenge1_database
  
  # To use both queries and complex queries
  swipl main.pl
  
  # To execute the queries
  ?- amdAfter2021PrintList.
  ?- amountOfCPASUSPrintList.
  ?- cpHDBetweenPrintList.
  ?- laptopsRamHDPrintList.
  ?- tablet2PlusGBRamPrintList.
  
  # To execute the complex queries
  ?- bestComputingPlatformPrint.
  ?- allBrandsMostCommonPrintList.
  ?- ramVRamMostCommonComboPrint.
  ?- worstComputingPlatformPrint.
  
  # To go back to the root :
  ctrl + c
  press e to exit
  cd ..

# 3) Enter challenge 2 Travel Planning System (TPS)
  cd challenge2_TPS

  # Load the main module
  swipl main.pl

  # Run example queries:
  ?- print_All(bogota, medellin).
  ?- print_All(bogota, medellin, 6, 7).
  ?- print_Cheapest(bogota, santa_marta).
  ?- print_Fastest(bogota, santa_marta).

  # To exit Prolog:
  ?- halt.
```
Made by: Mateo Montoya Ospina and Juan Pablo Lopez Lidueña







