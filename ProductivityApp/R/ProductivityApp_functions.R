#' @title Create an Empty Task Data Frame
#' @description Creates an empty task data frame. The data frame has 5 columns named Task, Duration, Importance, Urgency, and Enjoyability. It does not automatically convert strings into factors.
#' @return An empty data frame with the following columns:
#' \itemize{
#' \item \strong{Task}: the names of individual tasks
#' \item \strong{Duration}: the estimated duration of tasks
#' \item \strong{Importance}: how important the task is on a scale 1-4
#' \item \strong{Urgency}: how urgent the task is on a scale 1-4
#' \item \strong{Enjoyability}: how enjoyable the task is on a scale 1-3
#' } 
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
#' @return the updated task data frame with the following columns:
#' \itemize{
#' \item \strong{Task}: the names of individual tasks
#' \item \strong{Duration}: the estimated duration of tasks
#' \item \strong{Importance}: how important the task is on a scale 1-4
#' \item \strong{Urgency}: how urgent the task is on a scale 1-4
#' \item \strong{Enjoyability}: how enjoyable the task is on a scale 1-3
#' } 
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
#' @description Creates an Eisenhower matrix based on a data frame with tasks. Eisenhower matrix shows tasks' urgency on the x-axis and importance on the y-axis. It provides a good way to visualize tasks that should be prioritized.
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
      axis.title = ggplot2::element_text(size = 16, face = "bold"),  # Axis labels size
      axis.text = ggplot2::element_text(size = 16, face = "bold"),  # Axis tick labels size
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1.5)
    ) +
    ggplot2::geom_hline(yintercept = 2.5, linetype = "solid", color = "black", linewidth = 1) +  # Horizontal line
    ggplot2::geom_vline(xintercept = 2.5, linetype = "solid", color = "black", linewidth = 1)  # Vertical line
}

#' @title Order Task Data Frame
#' @description Orders the task data frame based on urgency, importance, and user preferences. Urgency takes precedence before importance and importance takes precedence before enjoyability.
#' @param df name of the task data frame
#' @param start_enjoy logical, whether user prefers to start with most or least enjoyable tasks that have equal urgency and importance.
#' @param start_short logical, whether user wants to start with short tasks (e.g. tasks that take less than 5 mins) regardless of their urgency, importance, and enjoyability.
#' @param short_time numeric, what task duration user considers to be short, this parameter is relevant if start_short is TRUE, default value is 5 minutes.
#' @return the ordered task data frame with the following columns:
#' \itemize{
#' \item \strong{Task}: the names of individual tasks
#' \item \strong{Duration}: the estimated duration of tasks
#' \item \strong{Importance}: how important the task is on a scale 1-4
#' \item \strong{Urgency}: how urgent the task is on a scale 1-4
#' \item \strong{Enjoyability}: how enjoyable the task is on a scale 1-3
#' }
#' @examples
#' # Order an example task data frame
#' example_df = data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording", "Make a dentist appointment for August", "Update my CV"),Duration = c(30, 60, 75, 3, 30),Importance = c(4, 3, 3, 2, 4),Urgency = c(4, 2, 3, 4, 4),Enjoyability = c(1, 3, 2, 1, 2),stringsAsFactors = FALSE)
#' order_task_df(example_df)
#' @export
order_task_df <- function(df, start_enjoy = FALSE, start_short = TRUE, short_time = 5){
  # First order tasks based on urgency, importance, and user's enjoyability preference
  ordered_tasks <- df %>%
    arrange(desc(Urgency), desc(Importance), if(start_enjoy) desc(Enjoyability) else Enjoyability)
  
  # Next, tasks with duration lower or equal to short_time will be moved up if 
  # the user prefers to start with short tasks even if they are not urgent/important
  if (start_short){
    ordered_tasks <- ordered_tasks %>%
      mutate(ShortTask = if_else(Duration <= short_time, 1, 0)) %>%
      arrange(desc(ShortTask)) %>%
      select(-ShortTask)
  }
  
  return(ordered_tasks)
}

#' @title Generate a To-Do List
#' @description Generates a to-do list based on a given task data frame. It uses the order_task_df function to first order the tasks based on urgency, importance and user preferences. Next, it assigns time intervals to each task and returns a data frame containing a to-do list with intervals and tasks.
#' @param df name of the task data frame
#' @param start_enjoy logical, whether user prefers to start with most or least enjoyable tasks that have equal urgency and importance.
#' @param start_short logical, whether user wants to start with short tasks (e.g. tasks that take less than 5 mins) regardless of their urgency, importance, and enjoyability.
#' @param short_time numeric, what task duration user considers to be short, this parameter is relevant if start_short is TRUE, default value is 5 minutes.
#' @param start_time character in a format "H:M", represents the time when the user wants to start working on their tasks.
#' @param available_time numeric, how much available time the user has (in minutes).
#' @param show_intervals logical, whether user wants to have time intervals next to every task
#' @return the to-do list data frame with time intervals and tasks
#' @examples
#' # Generate a to-do list based on this example task data frame
#' example_df = data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording", "Make a dentist appointment for August", "Update my CV"),Duration = c(30, 60, 75, 3, 30),Importance = c(4, 3, 3, 2, 4),Urgency = c(4, 2, 3, 4, 4),Enjoyability = c(1, 3, 2, 1, 2),stringsAsFactors = FALSE)
#' generate_todolist(example_df)
#' @export
generate_todolist <- function(df, start_enjoy = FALSE, start_short = TRUE, short_time = 5, start_time = "9:00", available_time = 480, show_intervals = TRUE){
  # Use the order_task_df helper function to order the task data frame
  df <- order_task_df(df, start_enjoy, start_short, short_time)
  
  # Get the start time
  start <- as.POSIXct(start_time, format = "%H:%M", tz = "UTC")
  
  # Create an empty character vector for task intervals
  task_intervals <- character(nrow(df))
  
  # Loop over tasks
  for (i in 1:nrow(df)) {
    # Assign start and end times for the current task
    end <- start + as.numeric(df$Duration[i]) * 60  # We need to convert duration from minutes to seconds
    interval <- paste(format(start, "%H:%M"), format(end, "%H:%M"), sep = "-")
    
    task_intervals[i] <- interval
    start <- end
  }
  
  # Add task_intervals to the dataframe
  df$Interval <- task_intervals
  
  if (show_intervals == TRUE){
    # I want to display only intervals and tasks
    df <- df %>% select(Interval, Task)
  }else{
    df <- df %>% select(Task)
  }

  return(df)
}

#' @title Generate a Productivity Plot
#' @description Generates a productivity plot with estimated and actual duration of individual tasks. Time is shown on the x-axis and tasks on the y-axis.
#' @param df name of the data frame with tasks and estimated and actual duration of each task
#' @return The productivity plot with estimated and actual duration of individual tasks.
#' @examples
#' # Generate a productivity plot based on an example data frame
#' example_df <- data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording"), Estimated_duration = c(30, 120, 90), Actual_duration = c(41, 200, 85))
#' get_productivity_plot(example_df)
#' @export
get_productivity_plot <- function(df){
  # Long data frame for easier plotting
  df_long <- data.frame(
    Task = rep(df$Task, 2),
    Type = c(rep("Estimated_duration", nrow(df)), rep("Actual_duration", nrow(df))),
    Duration = c(df$Estimated_duration, df$Actual_duration)
  )
  
  # Productivity plot
  ggplot(df_long, aes(x = Duration, y = Task, color = Type)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Estimated_duration" = "blue", "Actual_duration" = "red")) +
    labs(x = "Time (minutes)", y = " ", title = "Productivity Plot") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Title size
      axis.title = element_text(size = 16, face = "bold"),  # Axis labels size
      axis.text = element_text(size = 16, face = "bold"),  # Axis tick labels size
      panel.border = element_rect(colour = "black", fill = NA, size = 1.5)
    )
}

#' @title Generate a Productivity Report
#' @description Generates a productivity report with an overview of how long the user was productive and whether they tend to underestimate or overestimate the time it takes them to complete a task.
#' @param df name of the data frame with tasks and estimated and actual duration of each task
#' @return The productivity plot report with an overview of user's productivity.
#' @examples
#' # Generate a productivity plot based on an example data frame
#' example_df <- data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording"), Estimated_duration = c(30, 120, 90), Actual_duration = c(41, 200, 85))
#' productivity_report(example_df)
#' @export
productivity_report <- function(df){
  df <- df %>%
    filter(!is.na(Actual_duration)) %>%
    mutate(Estimation = ifelse(Actual_duration > Estimated_duration, "Underestimated",
                              ifelse(Actual_duration == Estimated_duration, "Perfect",
                                     "Overestimated")),
           Estimation_dif = Estimated_duration - Actual_duration) 
  
  productive_time <- round(sum(df$Actual_duration))
  productive_hours <- productive_time %/% 60
  productive_mins <- productive_time %% 60
  
  best_est <- min(abs(df$Estimation_dif))
  best_est_task <- df$Task[df$Estimation_dif == best_est | df$Estimation_dif == -best_est]
  
  report <- paste("In total, you've spent", productive_hours, "hours and", 
                  productive_mins, "minutes on your tasks. You were able to best estimate the duration of the task:", best_est_task)
  return(report)
}


