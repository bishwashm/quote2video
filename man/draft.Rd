% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draft.R
\name{draft}
\alias{draft}
\title{Create Draft Files for a Quote-to-Video Project}
\usage{
draft(project_name)
}
\arguments{
\item{project_name}{Character string, the name of the project directory to create.}
}
\value{
Invisible NULL, creates directories and file as a side effect.
}
\description{
This function creates a new directory with the specified project name in the working directory,
containing subdirectories (images, voiceovers, videos) and a draft quotes.csv file with sample data.
The user can modify the quotes.csv file manually before running build2video.
}
\examples{
draft("elon")
}
