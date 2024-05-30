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
  df <- df %>%
    dplyr::group_by(Urgency, Importance) %>%
    dplyr::mutate(
      y_offsets = seq(0, by = 0.2, length.out = dplyr::n()),  # Create sequence of offsets
      centered_y_offsets = y_offsets - mean(y_offsets),  # Center the offsets
      y_offset = if(dplyr::n() == 1) 0 else centered_y_offsets  # Apply centered offsets or fixed offset for single task
    ) %>%
    dplyr::ungroup()
  
  ggplot2::ggplot(df, ggplot2::aes(x = Urgency, y = Importance, label = Task)) +
    ggplot2::geom_point(color = "blue", size = 3, alpha = 0.2) +  # Points representing tasks
    ggplot2::geom_text(ggplot2::aes(y = Importance + y_offset), size = 5, hjust = 0.5, vjust = 1.5) +  # Labels for tasks
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
      dplyr::mutate(ShortTask = dplyr::if_else(Duration <= short_time, 1, 0)) %>%
      dplyr::arrange(desc(ShortTask)) %>%
      dplyr::select(-ShortTask)
  }
  
  return(ordered_tasks)
}

#' @title Check Tasks Given Availability
#' @description Generates a data frame and a message about unscheduled tasks for a to-do list based on user's time availability.   
#' @param df name of the ordered task data frame  
#' @param start_time start time in a format "%H:%M", when user wants to start working on their to-do list
#' @param end_time end time in a format "%H:%M", until when user wants to work on their to-do list
#' @return df and message:
#' #' \itemize{
#' \item \strong{Data Frame}: a data frame with tasks that can be scheduled based on user's availability.
#' \item \strong{Message}: a message about whether some tasks couldn't be scheduled - either saying "All tasks were successfully scheduled!" or "The following tasks were not scheduled because there was not enough time available:" followed by the names of tasks that could not be scheduled.
#' }
#' @examples
#' # Generate a data frame and a message about unscheduled tasks
#' example_df <- data.frame(Task = c("Meeting preparation", "Programming", "Watch lecture recording", "Make a dentist appointment for August", "Update my CV"),Duration = c(30, 60, 75, 3, 30),Importance = c(4, 3, 3, 2, 4),Urgency = c(4, 2, 3, 4, 4),Enjoyability = c(1, 3, 2, 1, 2),stringsAsFactors = FALSE)
#' example_ordered_df <- order_task_df(example_df)
#' tasks_given_availability(example_ordered_df)
#' @export
tasks_given_availability <- function(df, start_time, end_time){
  start_time <- as.POSIXct(start_time, format="%H:%M", tz = "UTC")
  end_time <- as.POSIXct(end_time, format="%H:%M", tz = "UTC")
  time_available <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  if (time_available == 0){
    message <- "The start and end time are the same, please revise it! The generated to-do list might not be optimal because it assumes that you have enough time available for all tasks from the start time that you put in." 
    # return(list(df = df, message = message))
  } else if (time_available < 0) {
    message <- "The end time is smaller than the start time, please revise it! The generated to-do list might not be optimal because it assumes that you have enough time available for all tasks from the start time that you put in." 
  } else if (df$Duration[1] > time_available) {
    message <- "There is a problem with your available time. It looks like you don't have enough time for the task that should be prioritized as first given the Eisenhower matrix and your preferences. Please revise the available time, task duration or your preferences so that the generated to-do list is optimal for you. Right now, the generated to-do list assumes that you have enough time available for all tasks from the start time that you put in which is probably not the case."
  } else if (sum(df$Duration) <= time_available){
    message <- "All tasks were successfully scheduled!"
  } else {
    task_fit <- FALSE
    unscheduled_tasks <- c()
    while(task_fit == FALSE){
      unscheduled_tasks <- c(unscheduled_tasks, df$Task[nrow(df)])
      df <- df[-nrow(df), ]
      if (sum(df$Duration) <= time_available){
        task_fit <- TRUE
      }
    }
    unscheduled_tasks_string <- paste(unscheduled_tasks, collapse = ", ")
    message <- paste("The following tasks were not scheduled because there was not enough time available:",
                     unscheduled_tasks_string)
  }
  
  return(list(df = df, message = message))
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
generate_todolist <- function(df, start_enjoy = FALSE, start_short = TRUE, short_time = 5, start_time = "09:00", end_time = "17:00", show_intervals = TRUE){
  # Use the order_task_df helper function to order the task data frame
  df <- order_task_df(df, start_enjoy, start_short, short_time)
  df <- tasks_given_availability(df, start_time, end_time)$df
  
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
    Type = rep(c("Estimated task duration", "Actual task duration"), each = nrow(df)),
    Duration = c(df$Estimated_duration, df$Actual_duration)
  )
  
  # Productivity plot
  ggplot2::ggplot(df_long, ggplot2::aes(x = Duration, y = Task, fill = Type, color = Type)) +
    ggplot2::geom_point(size = 3) +
    # ggplot2::scale_fill_manual(values =c("blue", "red")) + # This didn't work for some reason
    ggplot2::labs(x = "Time (minutes)", y = " ", title = "Productivity Plot") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),  # Title size
      axis.title = ggplot2::element_text(size = 16, face = "bold"),  # Axis labels size
      axis.text = ggplot2::element_text(size = 16, face = "bold"),  # Axis tick labels size
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1.5),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16, face = "bold")
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
    dplyr::filter(!is.na(Actual_duration)) %>%
    dplyr::mutate(Estimation = dplyr::if_else(Actual_duration > Estimated_duration, "Underestimated",
                              dplyr::if_else(Actual_duration == Estimated_duration, "Perfect",
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


