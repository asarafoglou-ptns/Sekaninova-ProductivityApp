% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProductivityApp_functions.R
\name{create_empty_task_df}
\alias{create_empty_task_df}
\title{Create an Empty Task Data Frame}
\usage{
create_empty_task_df()
}
\value{
An empty data frame with the following columns:
\itemize{
\item \strong{Task}: the names of individual tasks
\item \strong{Duration}: the estimated duration of tasks
\item \strong{Importance}: how important the task is on a scale 1-4
\item \strong{Urgency}: how urgent the task is on a scale 1-4
\item \strong{Enjoyability}: how enjoyable the task is on a scale 1-3
}
}
\description{
Creates an empty task data frame. The data frame has 5 columns named Task, Duration, Importance, Urgency, and Enjoyability. It does not automatically convert strings into factors.
}
\examples{
# Create an empty task data frame 
create_empty_task_df()
}
