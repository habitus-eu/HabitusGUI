library("HabitusGUI")
if ("data" %in% dir()) {
  homedir = paste0(getwd(),"/data") # cd to data directory
} else {
  homedir = getwd()
}
setwd(homedir)
HabitusGUI::myApp(homedir)
