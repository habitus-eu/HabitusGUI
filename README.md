![GitHub Actions R-CMD-check](https://github.com/habitus-eu/HabitusGUI/workflows/R-CMD-check-full/badge.svg)
[![codecov](https://codecov.io/gh/habitus-eu/HabitusGUI/branch/main/graph/badge.svg?token=GPRPJ3IXWC)](https://codecov.io/gh/habitus-eu/HabitusGUI)

# HabitusGUI


## 1 How to use app locally without docker

### 1.1 Install app  in R

```
library("remotes")
remotes::install_github("habitus-eu/HabitusGUI")
```

### 1.2 Install required other dependencies

For example, if you want to work with GGIR:

`install.packages("GGIR")`

### 1.3 Load HabitusGUI package

`library("HabitusGUI")`

### 1.4 Specify a directory that has all relevant data in it or in it's sub-directories

`data_dir = "/home/vincent/projects/fontys"`

### 1.5 Launch HabitusGUI app

`HabitusGUI::myApp(homedir=data_dir)`


## 2 Docker

### 2.1 How to use app locally with docker

This repository contains docker files needed for hosting Habitus Shiny app and
its dependencies.

1. Build app: `sudo docker build -t habitus-app .`

2. Run app and expose it to data volume on the host:

`sudo docker run --rm -v /home/vincent/projects/fontys:/srv/shiny-server/data -p 3838:3838 habitus-app`

Here `/home/vincent/projects/fontys` should be replaced by the local directory you would like to expose to the app.

(If you do `sudo docker run --rm -p 3838:3838 habitus-app` you would not expose the data to the app)

3. Open app in browser: `http://localhost:3838/`

4. Remove image: `sudo docker rmi habitus-app`

### 2.2 How to work with iso files

...


## 3 For tool developers


### 3.1 Expected information

To help us integrate your tool we need the following information from you:

- Docker file with linux installation instruction
- Single function call to interact with the package. This needs to be a function that takes as input the data location(s), configuration file, and output directory).
- Textual description of expected data input types, formats, and locations.
- .tsv file with for each parameter (rows) the following description fields (columns):
  - parameter: the name of the parameter
  - display: TRUE or FALSE, to indicate whether parameter should visible in the app
  - class: integer, double, set, or timezone
  - minimum: minimum value used if integer or double
  - maximum: maximum value used if integer or double
  - set: set of numbers or characters separated by a ';'
  - description: written description of the parameter that will be show to the app user
  - ... any other columns you would like to display, e.g. priority or parameter topic.
	
### 3.2 What expect we to handle inside your tool

- Parallel processing functionality if relevant
- Handling of corrupt or invalid data files, and clear communication about the identification of such files with the user.
- Option to use a configuration file to controll all tcore functionality of the tool. 