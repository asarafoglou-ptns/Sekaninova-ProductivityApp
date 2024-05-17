#' @title Create an Empty Task File
#' @description Creates an empty csv file with a default name tasks.csv. The file has 5 columns named task, duration, importance, urgency, and enjoyability.
#' @param filename string with a default value "tasks.csv"
#' @return csv file is created
#' @examples
#' # Create an empty task csv file in your current working directory
#' create_empty_task_csv()
#' @export
create_empty_task_csv <- function(filename = "tasks.csv") {
  # Creating an empty data frame with specified column names
  data <- data.frame(
    task = character(),
    duration = numeric(),
    importance = numeric(),
    urgency = numeric(),
    enjoyability = numeric()
  )
  
  # Writing the data frame to a CSV file
  write.csv(data, file = filename, row.names = FALSE)
}

#' @title Add Task 
#' @description Adds a new task with its variables to the tasks.csv file. 
#' @param task character, name of the task you want to add
#' @param duration integer, duration of the task in minutes
#' @param importance integer (1-4), how important the task is
#' @param urgency integer(1-4), how urgent the task is
#' @param enjoyability integer(1-3), how enjoyable the task is
#' @return the tasks.csv file is updated
#' @examples
#' # Add a task to your task file
#' add_task(task = "Programming", duration = 180, importance = 4, urgency = 3, enjoyability = 2)
#' @export
add_task <- function(task, duration, importance, urgency, enjoyability) {
  # Load tasks' data from the existing csv file
  tasks_data <- read.csv("tasks.csv")
  
  # Add new task data to the csv file
  new_row <- data.frame(
    task = task,
    duration = duration,
    importance = importance,
    urgency = urgency,
    enjoyability = enjoyability
  )
  
  tasks_data <- rbind(tasks_data, new_row)
  write.csv(tasks_data, file = "tasks.csv", row.names = FALSE)
}

#' @title Generate an Eisenhower Matrix
#' @description Creates an Eisenhower matrix based on a file with tasks.
#' @param filename character, name of the task csv file, default is "tasks.csv"
#' @return the Eisenhower matrix
#' @examples
#' # Generate an Eisenhower matrix, assuming you have "tasks.csv" file in your working directory
#' generate_eisenhower_matrix()
#' @export
generate_eisenhower_matrix <- function(filename = "tasks.csv"){
  # NEEDS IMPROVEMENT
  tasks_data <- read.csv(filename)
  
  plot <- ggplot2::ggplot(tasks_data, ggplot2::aes(x = urgency, y = importance, label = task)) +
    ggplot2::geom_point(color = "blue", size = 3) +  # Points representing tasks
    ggplot2::geom_text(size = 3, hjust = -0.2, vjust = -0.5) +  # Labels for tasks
    ggplot2::xlim(0, 4) + ggplot2::ylim(0, 4) +  # Set limits for x and y axes
    ggplot2::labs(title = "Eisenhower Matrix", x = "Urgency", y = "Importance") +  # Axis labels and plot title
    ggplot2::theme_minimal()  # Minimal theme for the plot
  
  print(plot)
}

# update_preferences <- function(free_time = 360, start_task_under_5 = TRUE, start_enjoyable = FALSE) {
#   # HERE MAYBE CREATE A CSV FILE WITH PREFERENCES ? OR JUST A DATAFRAME
# }
# 
# generate_todolist <- function(){
#   # generate todolist based on preferences 
# }
