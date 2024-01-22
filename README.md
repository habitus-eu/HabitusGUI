![GitHub Actions R-CMD-check](https://github.com/habitus-eu/HabitusGUI/workflows/R-CMD-check-full/badge.svg)

HabitusGUI is a Shiny app designed to ease processing behavioural data with research software such as GGIR, activityCounts, PALMSpy and PALMSplus. If you want to contribute to HabitusGUI then please find our contributing guidelines [here](https://github.com/wadpac/GGIR/blob/master/CONTRIBUTING.md).

## 1 Instructions for tool contributors

If you are the maintainer of a tool that has been or needs to be embedded in HabitusGUI then please see instructions   [here](https://github.com/habitus-eu/HabitusGUI/blob/main/INSTRUCTIONS_TOOL_MAINTAINERS.md).

## 2 Installation instructions

### 2.1 Install R

Install R: https://cran.r-project.org/
Install RStudio: https://posit.co/products/open-source/rstudio/

### 2.2 Install tools

Install the tools you plan to use:
- [install GGIR](https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html#2_Setting_up_your_work_environment)
- [install hbGPS](https://github.com/habitus-eu/hbGPS)
- [install hbGIS](https://github.com/habitus-eu/hbGIS)

### 2.3 Install HabitusGUI

HabitusGUI can be installed as R package with the following commands in the RStudio console:

```
install.packages(remotes); library("remotes"); remotes::install_github("habitus-eu/HabitusGUI", dependencies=TRUE)
```

#### 2.4 Using HabitusGUI

1. Start RStudio
2. In the RStudio console type: `library("HabitusGUI")`
3. Specify a directory that has all relevant data in it's root or sub-directories:

`data_dir = "/home/vincent/projects/fontys"`

4. Tell R to us ignore sp evoluation warnings and use sf instead:

`options("sp_evolution_status" = 2)`

5. Launch HabitusGUI app:

`HabitusGUI::myApp(homedir=data_dir)`

