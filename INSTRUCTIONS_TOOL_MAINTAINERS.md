# Instructions for tool maintainers

This section describes what we expect from the maintainers of the external software tools that are used by HabitusGUI.

### 1 Information we need from you about your software tool

To help us integrate your tool we need the following information from you:

1. Docker file with linux installation instruction
2. Installation instruction for local installation without docker
3. Single function call to interact with the package. This needs to be a function that takes as input the data location(s), configuration file, and output directory).
4. Textual description of expected data input types, formats, and locations.
5. .tsv file with for each parameter (rows) the following description fields (columns):
    - parameter: the name of the parameter
    - display: TRUE or FALSE, to indicate whether parameter should visible in the app
    - class: integer, double, set, or timezone
    - minimum: minimum value used if integer or double
    - maximum: maximum value used if integer or double
    - set: set of numbers or characters separated by a ';'
    - timezone: a timezone database name, also known as Olson names.
    - description: written description of the parameter that will be show to the app user
    - ... any other columns you would like to display, e.g. priority or parameter topic.
6. Example configuration file that the software will accept. In .json or .csv format would be easier.
7. Example input files, if not included in the software.
8. List of research goals for which the tool is needed.
9. If software tool takes as input the output from other software tools then describe what those other tools are and how they would have to be configured.

Note: Please try to keep the above consistent across future releases of your tool. If you have to make a change then please let us know in time such that we can update our end of the code accordingly.

### 2 What we expect from your software tool

1. Is publicly available and has Open Source License file.
2. Has an integration test to demonstrate that the software can be uninstalled.
3. Has some unit test to test for core functionalities.
4. Handles corrupt or invalid data files, and provides clear communication about the identification of such files with the user.
5. Allows for using a configuration file to set all software parameters relevant to the end-user.
6. To take advantage of parallel processing on CPU infrastructure if relevant.