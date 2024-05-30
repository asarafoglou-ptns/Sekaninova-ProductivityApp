# Sekaninova-ProductivityApp
R package to generate to-do lists based on different task characteristics and user preferences. Users can go through their to-do lists using Pomodoro technique and generate a Productivity report once they complete their tasks. 

You need to run the following lines first unless you have some of the packages already installed:
devtools::install_github("asarafoglou-ptns/Sekaninova-ProductivityApp/ProductivityApp")
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("DT")
install.packages("tidyverse")
install.packages("shinyjs")

If you have all necessary packages installed, you can run the app directly from github by pasting these two lines into your console:
devtools::source_url("https://raw.githubusercontent.com/asarafoglou-ptns/Sekaninova-ProductivityApp/main/app.R")
shiny::shinyApp(ui, server)

Please read the README.pdf file that goes into more detail on how to use the app. 
