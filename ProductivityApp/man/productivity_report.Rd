% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProductivityApp_functions.R
\name{productivity_report}
\alias{productivity_report}
\title{Generate a Productivity Report}
\usage{
productivity_report(df)
}
\arguments{
\item{df}{name of the data frame with tasks and estimated and actual duration of each task}
}
\value{
The productivity plot report with an overview of user's productivity.
}
\description{
Generates a productivity report with an overview of how long the user was productive, whether they tend to underestimate or overestimate the time it takes them to complete a task, and which task or tasks they estimated the best.
}
\examples{
# Generate a productivity plot based on an example data frame
example_df <- data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording"), Estimated_duration = c(30, 120, 90), Actual_duration = c(41, 200, 85))
productivity_report(example_df)
}
