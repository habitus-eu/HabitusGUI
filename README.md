![GitHub Actions R-CMD-check](https://github.com/habitus-eu/HabitusGUI/workflows/R-CMD-check-full/badge.svg)
[![codecov](https://codecov.io/gh/habitus-eu/HabitusGUI/branch/main/graph/badge.svg?token=GPRPJ3IXWC)](https://codecov.io/gh/habitus-eu/HabitusGUI)

HabitusGUI is a Shiny app designed to ease processing behavioural data with research software such as GGIR, activityCounts, PALMSpy and PALMSplus. If you are the maintainer of one of these software tools then please see our requirements on your tool [here](https://github.com/habitus-eu/HabitusGUI/INSTRUCTIONS_TOOL_MAINTAINERS.md).


We anticipate that HabitusGUI will be installed on a server environment, where the typical user will not have to worry about the installation process as described below.

## 1 Install without Docker

This approach assumes that all HabitusGUI dependencies, such as GGIR, PALMSpy, and PALMSplus, are available in the work environment.

#### 1.1 Install for the first time:

1. Install R: https://cran.r-project.org/
2. Start R: `R`
3. Copy and paste to the following line to the R command line and press Enter:

```
install.package(remotes); library("remotes"); remotes::install_github("habitus-eu/HabitusGUI", dependencies=TRUE)
```

If you ever want to update the software in the future then repeat this step.

#### 1.2 Using HabitusGUI

1. Start R
2. In the R command line type: `library("HabitusGUI")`
3. Specify a directory that has all relevant data in it's root or sub-directories:

`data_dir = "/home/vincent/projects/fontys"`

4. Launch HabitusGUI app: `HabitusGUI::myApp(homedir=data_dir)`

## 2 Install with Docker

The HabitusGUI R package repository contains docker files needed for hosting Habitus Shiny app and
its dependencies.

#### 2.1 First time installation

1. Install docker via the official installation instructions: https://docs.docker.com/get-docker/.

Note for VirtualBox users in Windows: When working in Windows, the installation directs you to the installation of the "Docker Desktop for Windows". This is not compatible with VirtualBox. We recommend that you use VirtualBox to launch a Ubuntu VM and to launch the app from within the VM.

2. Create a folder on your machine an name it HabitusDocker

3. Go to https://github.com/habitus-eu/HabitusGUI/blob/main/Docker/Dockerfile and right click on Raw, and then left click on `Save link as ...` Save the file to the HabitusDocker folder.

4. Create a folder in the same directory as where you stored the Docker file and name it 'code' without quotes.

5. Go to https://github.com/habitus-eu/HabitusGUI/blob/main/Docker/code/app.R and right click on Raw, and then left click on `Save link as...`. Save the file in the folder 'code' you use created.

6. When in Windows: Open Windows Powershell.
When in Linux: Open Linux Command prompt.

7. cd to the folder HabitusDocker folder

8. Type `docker build -t habitus-app .`


#### 2.2 Run the installed HabitusGUI app with "Docker Desktop for Windows"

1. Open the "Docker Desktop for Windows"
2. Go to images
3. Click run
4. Click on icon to launch in browser


#### 2.3 Run the installed HabitusGUI app without "Docker Desktop for Windows"

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


#### 2.4 Remove HabitusGUI image

If you would ever when to remove the image then do:

1. **Windows:** Open Windows Powershell. **Linux:** Open Linux Command prompt.

2. `docker rmi habitus-app`

