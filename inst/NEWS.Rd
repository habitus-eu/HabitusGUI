\name{NEWS}
\title{News for Package \pkg{HabitusGUI}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\section{Changes in version 0.3.2 (GitHub-only-release date: 20-03-2024)}{
  \itemize{
    \item Changed default for save_ms5rawlevels in template config.csv to TRUE
    \item Fix issue with old activityCounts dependency that we was not 
    correctly deprecated in the previous release
    \item Add hbGIS parameter documentation for use inside the GUI
  }
}
\section{Changes in version 0.3.1 (GitHub-only-release date: 22-01-2024)}{
  \itemize{
    \item Migrate all palmsplusr functionality hbGIS package, see issue #64. 
    \item Deprecate PALMSpy and count converter, see issue #98
  }
}
\section{Changes in version 0.3.0 (GitHub-only-release date: 28-09-2023)}{
  \itemize{
    \item Series of fixes based on initial testing phase, see issue #94. 
  }
}
\section{Changes in version 0.2.0 (GitHub-only-release date: 15-09-2023)}{
  \itemize{
    \item Embed hbGPS #90
    \item Allow for hiding progress log view #46
  }
}
\section{Changes in version 0.1.9 (GitHub-only-release date: 08-09-2023)}{
  \itemize{
    \item Deprecate saving configfile in Shiny state as currently functionality
    was not working well (e.g. it did not allow for overwriting previous configfile)
    and to reduce code complexity #83
    \item Create error when expected GIS file cannot be found #87.
    \item Improved compatibility of palmsplusr functions with hbGPS output
  }
}
\section{Changes in version 0.1.8 (GitHub-only-release date: 30-06-2023)}{
  \itemize{
    \item Allowing package user to specify the local conda environment location needed for PALMSpy
    \item Shiny state is saved and used if available when refreshing the app.
  }
}
\section{Changes in version 0.1.7 (GitHub-only-release date: 01-05-2023)}{
  \itemize{
    \item Revised tsv file for GGIR configuration
    \item Testfile for config GGIR updated
    \item Now able to handle acc.metric in GGIR
    \item HabitusGUI and use-case GGIR can now run again in Windows, this was broken ince 0.1.5 release
  }
}
\section{Changes in version 0.1.6 (GitHub-only-release date: 15-03-2023)}{
  \itemize{
    \item GGIR config file is now checked when it is loaded
    \item Removing references to Docker from documentation
  }
}
\section{Changes in version 0.1.5 (GitHub-only-release date: 20-12-2022)}{
  \itemize{
    \item PALMSpy now run via persistent command line call (PR #58)
    \item BrondCounts deprecated and replaced by Neishabouricounts
    \item GGIR now run via persistent command line call (issue #60)
    \item palmsplusr integration imporved by restructuring code
    \item palmsplusr integration of GIS files simplified by extracting locations
    from GIS files (issue #62)
  }
}
\section{Changes in version 0.1.4 (GitHub-only-release date: 26-07-2022)}{
  \itemize{
    \item Improved integrated of palmsplusr which now also uses GIS files
  }
}
\section{Changes in version 0.1.3 (GitHub-only-release date: 16-06-2022)}{
  \itemize{
    \item palmsplusr integrated based on default config file.
  }
}
\section{Changes in version 0.1.2 (GitHub-only-release date: 08-12-2021)}{
  \itemize{
    \item PALMSpy replaced by real version, currently assumes that PALMSpy is available from root
  }
}
\section{Changes in version 0.1.1 (GitHub-only-release date:25-11-2021)}{
  \itemize{
    \item GGIR and activityCounts integrated
    \item PALMSpy integrated but with dummy function for now
    \item Configuration page now auto-checks parameter values and provides feedback to user
    \item Analysis page now suggests order of tools and provides feedback to user when doing it wrong.
    \item Documentation added for tool maintainers
    \item Installation instruction added to README
    \item CONTRIBUTING file added
  }
}
\section{Changes in version 0.1-0 (GitHub-only-release date:06-09-2021)}{
  \itemize{
    \item First release with GGIR and a few dummy tools to help develop functionality.
  }
}
