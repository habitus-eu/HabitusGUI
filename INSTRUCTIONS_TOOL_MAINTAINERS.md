# Instructions for tool maintainers

HabitusGUI aims to facilitate the embedding of both Python and R libraries. To ease the effort of embedding libraries and the maintenance, we have generated a list of items we need from you and have clarified what we expect that your software is capable of.

### 1 Information we need from you

1. Installation instruction for local installation without docker
2. Single function call to interact with your library. This needs to be a function that takes as input the data location(s), configuration file, and output directory where results are to be stored.
3. Description of expected data input types, formats, and if relevant the expected folder structure. Also explain which combinations of available data are possible. 
4. If software tool takes as input the output from other software tools then describe what those other tools are and how they would have to be configured.
5. Example input files, if not included in the software.
6. Example configuration file that the software will accept. In .json or .csv format would be easier.
6. .tsv file with for each configuration parametes the following description fields (columns):
    - parameter: the name of the parameter
    - display: TRUE or FALSE, to indicate whether parameter should visible to the HabitusGUI user
    - class: integer, double, set, or timezone. Note that for timezone, we assume that timezone is stored as Olson timezone database name.
    - minimum: minimum value used if integer or double
    - maximum: maximum value used if integer or double
    - set: set of numbers or characters separated by a ';'. If you use a Boolean value in json format then specify TRUE;FALSE, even if the actual value in the json file is true or false.
    - description: written description of the parameter that will be show to the app user
    - priority: 1 or 0, where 1 will be highlighted in yellow.
    - ... any other columns you would like to display, e.g. priority or parameter topic.
7. List of research goals for which the tool is needed.

Note: Please try to keep the above consistent across future releases of your tool. If you have to make a change then please let us know in time such that we can update HabitusGUI accordingly.

### 2 What we expect from your software tool

1. Publicly available with Open Source License file.
2. Has an integration test to demonstrate that the software can be installed.
3. Has some unit test to test for core functionality.
4. Handles corrupt or invalid data files, and provides clear communication to the user about the identification of such files.
5. Allows for using a configuration file to set all software parameters relevant to the end-user.
6. If relevant, takes advantage of parallel processing on CPU infrastructure. HabitusGUI does not do this for you.