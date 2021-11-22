![GitHub Actions R-CMD-check](https://github.com/habitus-eu/HabitusGUI/workflows/R-CMD-check-full/badge.svg)
[![codecov](https://codecov.io/gh/habitus-eu/HabitusGUI/branch/main/graph/badge.svg?token=GPRPJ3IXWC)](https://codecov.io/gh/habitus-eu/HabitusGUI)

# 1 Installation

We anticipate that HabitusGUI will be installed on a server environment, where the typical user will not have to worry about the installation process. The instructions below are primarily created to allow for testing purposes.

## 1.1 Install locally without Docker

This approach assumes that all HabitusGUI dependencies, such as GGIR, PALMSpy, and PALMSplus, are available in the work environment.

#### 1.1.1 Install for the first time:

1. Install R: https://cran.r-project.org/
2. Start R: `R`
3. Copy paste to the following line to the R command line and press Enter:

```
install.package(remotes); library("remotes"); remotes::install_github("habitus-eu/HabitusGUI", dependencies=TRUE)
```

If you ever want to update the software in the future then repeat this step.

#### 1.1.2 Using HabitusGUI

1. Start R
2. In the R command line type: `library("HabitusGUI")`
3. Specify a directory that has all relevant data in it's root or sub-directories:

`data_dir = "/home/vincent/projects/fontys"`

4. Launch HabitusGUI app: `HabitusGUI::myApp(homedir=data_dir)`

## 1.2 Docker

The HabitusGUI R package repository contains docker files needed for hosting Habitus Shiny app and
its dependencies.

#### 1.2.1 First time installation

1. Install docker via the official installation instructions: https://docs.docker.com/get-docker/.

Note for VirtualBox users in Windows: When working in Windows, the installation directs you to the installation of the 'Docker Desktop for Windows'. This is not compatible with VirtualBox. We recommend that you use VirtualBox to launch a Ubuntu VM and to launch the app from within the VM.

2. Create a folder on your machine an name it HabitusDocker

3. Go to https://github.com/habitus-eu/HabitusGUI/blob/main/Docker/Dockerfile and right click on Raw, and then left click on `Save link as ...` Save the file to the HabitusDocker folder.

4. Create a folder in the same directory as where you stored the Docker file and name it 'code' without quotes.

5. Go to https://github.com/habitus-eu/HabitusGUI/blob/main/Docker/code/app.R and right click on Raw, and then left click on `Save link as...`. Save the file in the folder 'code' you use created.

6. When in Windows: Open Windows Powershell.
When in Linux: Open Linux Command prompt.

7. cd to the folder HabitusDocker folder

8. Type `docker build -t habitus-app .`


#### 1.2.2 Run the installed HabitusGUI app  with Docker Desktop for Windows

1. Open the 'Docker Desktop for Windows'
2. Go to images
3. Click run
4. Click on icon to launch in browser


#### 1.2.3 Run the installed HabitusGUI app without Docker Desktop for Windows

1. Open Command line:

   **In Windows:** Open Windows Powershell

   **In Linux:** Open Linux Command prompt.

2. cd to the folder HabitusDocker folder as created earlier

3. Run app and expose it to data volume on the host:

   **In Linux:** `docker run --rm -v /home/vincent/projects/fontys:/srv/shiny-server/data -p 3838:3838 habitus-app`

   Here `/home/vincent/projects/fontys` should be replaced by the local directory you would like to expose to the app.

   **In Windows:** `docker run --rm -v D:\Docker:/srv/shiny-server/data -p 3838:3838 habitus-app`

   Here `D:\Docker` should be replaced by the local directory you would like to expose to the app.

4. Open app in browser: `http://localhost:3838/`


#### 1.2.4 Remove HabitusGUI image

If you would ever when to remove the image then do:

1. **Windows:** Open Windows Powershell. **Linux:** Open Linux Command prompt.

2. `docker rmi habitus-app`


# 2 Instructions for tool contributors

This paragraph describes what we expect from the maintainers of the tools that are used by HabitusGUI

### 2.1 Information we need from you

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
  - description: written description of the parameter that will be show to the app user
  - ... any other columns you would like to display, e.g. priority or parameter topic.
6. Example configuration file that the software will accept. In .json or .csv format would be easier.
7. Example input files, if not included in the software.
8. List of research goals for which the tool is needed.
9. If software tool takes as input the output from other software tools then describe what those other tools are and how they would have to be configured.


Note: Please try to keep the above consistent across releases of your tool. If you have to make a change then please let us know in time such that we can update our end of the code accordingly.

### 2.2 What expect from your software

1. Is Open Source and has an Open Source License file in the repository
2. Has an integration test to show that software can be installated.
3. Handles corrupt or invalid data files, and provides clear communication about the identification of such files with the user.
4. Allows for controlling all software parameters relevant to the research with a configuration file.
5. To take advantage of parallel processing on CPU infrastructure.
4. Integration test
