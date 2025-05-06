# quote2video 
# quote2video

An R package to create videos from quotes by generating images, voice-overs, and combining them into a final video.

## Installation

Install the package using `devtools`:

```R
# Install devtools if not already installed
install.packages("devtools")
devtools::install_github("bishwashm/quote2video")
Usage
Create a project directory with sample quotes:
library(quote2video)
draft("myproject")
Add a background.jpg to the project directory (myproject/), and edit quotes.csv with your quotes.
Build the video:
build2video("myproject")
Requirements
R packages: readr, dplyr, purrr, reticulate, jpeg (or png)
FFmpeg installed and added to PATH
Python with gTTS installed (pip install gTTS)
A music.mp3 file in the parent directory of your project
Notes
Ensure background.jpg is a valid JPG or PNG file.
The final video will be saved as myproject/final_video.mp4.
