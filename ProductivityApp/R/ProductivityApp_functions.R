#' @title Create an Empty Task Data Frame
#' @description Creates an empty task data frame. The data frame has 5 columns named Task, Duration, Importance, Urgency, and Enjoyability. It does not automatically convert strings into factors.
#' @return An empty data frame with columns Task, Duration, Importance, Urgency, and Enjoyability.
#' @examples
#' # Create an empty task data frame 
#' create_empty_task_df()
#' @export
create_empty_task_df <- function() {
  # Creating an empty data frame with specified column names
  data.frame(
    Task = character(),
    Duration = numeric(),
    Importance = numeric(),
    Urgency = numeric(),
    Enjoyability = numeric(),
    stringsAsFactors = FALSE
  )
}

#' @title Add Task 
#' @description Adds a new task with its variables to the task data frame with columns Task, Duration, Importance, Urgency, and Enjoyability.
#' @param df name of the task data frame that will be updated
#' @param task character, name of the task you want to add
#' @param duration integer, duration of the task in minutes
#' @param importance integer (1-4), how important the task is
#' @param urgency integer(1-4), how urgent the task is
#' @param enjoyability integer(1-3), how enjoyable the task is
#' @param shiny Boolean, whether this function will be used with shiny's reactive dataframe, default is TRUE 
#' @return the updated task data frame
#' @examples
#' # Add a task to an example data frame
#' example_df = data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording"),Duration = c(30, 60, 75),Importance = c(4, 3, 3),Urgency = c(4, 2, 3),Enjoyability = c(1, 3, 2),stringsAsFactors = FALSE)
#' add_task(df = example_df, task = "Programming", duration = 180, importance = 4, urgency = 3, enjoyability = 2, shiny = FALSE)
#' @export
add_task <- function(df, task, duration, importance, urgency, enjoyability, shiny = TRUE) {
  # Add new task data to the task data frame
  new_row <- data.frame(
    Task = task,
    Duration = duration,
    Importance = importance,
    Urgency = urgency,
    Enjoyability = enjoyability,
    stringsAsFactors = FALSE
  )
  if (shiny == TRUE){
    df(rbind(df(), new_row))
  } else {
    rbind(df, new_row)
  }
}

#' @title Generate an Eisenhower Matrix
#' @description Creates an Eisenhower matrix based on a data frame with tasks.
#' @param df name of the task data frame
#' @return the Eisenhower matrix
#' @examples
#' # Generate an Eisenhower matrix based on an example data frame
#' example_df = data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording"),Duration = c(30, 60, 75),Importance = c(4, 3, 3),Urgency = c(4, 2, 3),Enjoyability = c(1, 3, 2),stringsAsFactors = FALSE)
#' generate_eisenhower_matrix(df = example_df)
#' @export
generate_eisenhower_matrix <- function(df){
  ggplot2::ggplot(df, ggplot2::aes(x = Urgency, y = Importance, label = Task)) +
    ggplot2::geom_point(color = "blue", size = 3) +  # Points representing tasks
    ggplot2::geom_text(size = 5, hjust = 0.5, vjust = 1.5) +  # Labels for tasks
    ggplot2::xlim(0.5, 4.5) + ggplot2::ylim(0.5, 4.5) +  # Set limits for x and y axes
    ggplot2::labs(title = "Eisenhower Matrix", x = "Urgency", y = "Importance") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),  # Title size
      axis.title = ggplot2::element_text(size = 16),  # Axis labels size
      axis.text = ggplot2::element_text(size = 16)  # Axis tick labels size
    ) +
    ggplot2::geom_hline(yintercept = 2.5, linetype = "solid", color = "black", linewidth = 1) +  # Horizontal line
    ggplot2::geom_vline(xintercept = 2.5, linetype = "solid", color = "black", linewidth = 1)  # Vertical line
}

# update_preferences <- function(free_time = 360, start_task_under_5 = TRUE, start_enjoyable = FALSE) {
#   # not sure if I need this function
# }
# 
# generate_todolist <- function(){
#   # still need to develop this function
#   # generate todolist based on preferences 
# }
