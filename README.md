![GitHub Actions R-CMD-check](https://github.com/habitus-eu/HabitusGUI/workflows/R-CMD-check-full/badge.svg)
[![codecov](https://codecov.io/gh/habitus-eu/HabitusGUI/branch/main/graph/badge.svg?token=GPRPJ3IXWC)](https://codecov.io/gh/habitus-eu/HabitusGUI)

# HabitusGUI


## 1. How to use app locally without docker

### 1.1 Install app

```
library("remotes")
%>% remotes::install_github("habitus-eu/HabitusGUI")
```

### 2. Install required other dependencies

For example, if you want to work with GGIR:

`install.packages("GGIR")`

### 2. Load app

`library("HabitusGUI")`

### 3. Specify a directory that has all relevant data in it or in it's sub-directories

`data_dir = "/home/vincent/projects/fontys"`

### 4. Launch app

`HabitusGUI::myApp(homedir=data_dir)`


## 2. How to use app locally with docker

Documentation to follow...
