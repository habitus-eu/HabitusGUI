# Instructions for tool maintainers

HabitusGUI aims to facilate the embedding of both Python and R libraries. To ease the effort of embedding libraries and the maintenance, we have generated a list of items we need from you and have clarified what we expect that your software is capable of.

### 1 Information we need from you

1. Installation instruction for local installation without docker
2. Docker file with linux installation instruction
3. Single function call to interact with your library. This needs to be a function that takes as input the data location(s), configuration file, and output directory where results are to be stored.
4. Description of expected data input types, formats, and if relevant the expected folder structure. Also explain which combinations of available data are possible. 
5. If software tool takes as input the output from other software tools then describe what those other tools are and how they would have to be configured.
6. Example input files, if not included in the software.
7. Example configuration file that the software will accept. In .json or .csv format would be easier.
8. .tsv file with for each configuration parametes the following description fields (columns):
    a. parameter: the name of the parameter
    b. display: TRUE or FALSE, to indicate whether parameter should visible to the HabitusGUI user
    c. class: integer, double, set, or timezone
    d. minimum: minimum value used if integer or double
    e. maximum: maximum value used if integer or double
    f. set: set of numbers or characters separated by a ';'
    g. timezone: a timezone database name, also known as Olson names.
    h. description: written description of the parameter that will be show to the app user
    i. ... any other columns you would like to display, e.g. priority or parameter topic.
9. List of research goals for which the tool is needed.

Note: Please try to keep the above consistent across future releases of your tool. If you have to make a change then please let us know in time such that we can update HabitusGUI accordingly.

### 2 What we expect from your software tool

1. Publicly available and has Open Source License file.
2. Has an integration test to demonstrate that the software can be installed.
3. Has some unit test to test for core functionality.
4. Handles corrupt or invalid data files, and provides clear communication to the user about the identification of such files.
5. Allows for using a configuration file to set all software parameters relevant to the end-user.
6. If relevant, takes advantage of parallel processing on CPU infrastructure. HabitusGUI does not do this for you.