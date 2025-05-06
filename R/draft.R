#' Create Draft Files for a Quote-to-Video Project
#'
#' This function creates a new directory with the specified project name in the working directory,
#' containing subdirectories (images, voiceovers, videos) and a draft quotes.csv file with sample data.
#' The user can modify the quotes.csv file manually before running build2video.
#'
#' @param project_name Character string, the name of the project directory to create.
#' @return Invisible NULL, creates directories and file as a side effect.
#' @export
#'
#' @examples
#' draft("elon")
draft <- function(project_name) {
  # Store the initial working directory
  initial_wd <- getwd()

  # Create the project directory at the top level of the initial working directory
  project_dir <- file.path(initial_wd, project_name)
  dir.create(project_dir, showWarnings = FALSE)
  if (!dir.exists(project_dir)) stop("Failed to create project directory: ", project_dir)

  # Create subdirectories inside the project directory
  subdirs <- c("images", "voiceovers", "videos")
  for (dir in subdirs) {
    dir_path <- file.path(project_dir, dir)
    dir.create(dir_path, showWarnings = FALSE)
    message("Created directory: ", dir_path)
  }

  # Create draft quotes.csv with sample data inside the project directory
  sample_data <- data.frame(
    Quote = c("The best way to predict the future is to create it.", "Success is not final, failure is not fatal."),
    Author = c("Peter Drucker", "Winston Churchill")
  )
  quotes_path <- file.path(project_dir, "quotes.csv")
  write.csv(sample_data, quotes_path, row.names = FALSE)
  message("Created draft quotes.csv at ", quotes_path, ". Please modify it as needed and add background.jpg to ", project_dir)

  invisible(NULL)
}
