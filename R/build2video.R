#' Build Video from Quotes
#'
#' This function compiles a video by generating images, voice-overs, individual videos,
#' and the final combined video using files from the specified project directory.
#' The final video is saved in the project directory.
#'
#' @param project_name Character string, the name of the project directory to use.
#' @return Invisible NULL, creates the final video as a side effect.
#' @export
#'
#' @examples
#' build2video("elon")
build2video <- function(project_name) {
  # Load required libraries (assumed installed)
  library(readr)
  library(dplyr)
  library(purrr)
  library(reticulate)

  # Store the initial working directory
  initial_wd <- getwd()

  # Set working directory to the project directory
  project_dir <- file.path(initial_wd, project_name)
  if (!dir.exists(project_dir)) stop("Project directory not found: ", project_dir)
  setwd(project_dir)

  # Read quotes from CSV and validate
  quotes_path <- "quotes.csv"
  quotes <- tryCatch({
    read_csv(quotes_path, show_col_types = FALSE)  # Suppress column type message
  }, error = function(e) {
    stop("Failed to read ", quotes_path, ": ", e$message)
  })

  if (!all(c("Quote", "Author") %in% colnames(quotes))) {
    stop(quotes_path, " must have 'Quote' and 'Author' columns")
  }

  # Add paths for images, voice-overs, and videos (relative to project_dir)
  quotes <- quotes %>%
    mutate(
      image_file = file.path("images", paste0("quote_", row_number(), ".png")),
      voiceover_file = file.path("voiceovers", paste0("voiceover_", row_number(), ".mp3")),
      video_file = file.path("videos", paste0("quote_", row_number(), ".mp4"))
    )

  # Verify FFmpeg installation
  if (system("ffmpeg -version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("FFmpeg not found. Ensure FFmpeg is installed and added to PATH.")
  }

  # Verify background.jpg exists
  background_path <- "background.jpg"
  if (!file.exists(background_path)) {
    stop("background.jpg not found in ", project_dir, ". Please upload it manually.")
  }

  # --- Automate Text-to-Speech ---
  use_python("C:/Users/bishw/AppData/Local/Programs/Python/Python313/python.exe", required = TRUE)
  gtts <- import("gtts")

  generate_voiceover <- function(quote, author, output_file) {
    text_to_speak <- paste0(quote, " by ", author)
    tts <- gtts$gTTS(text = text_to_speak, lang = "en", slow = FALSE)
    tts$save(output_file)
    message("Generated voice-over: ", output_file)
  }

  message("Generating voice-overs...")
  tryCatch({
    pmap(list(quotes$Quote, quotes$Author, quotes$voiceover_file), generate_voiceover)
  }, error = function(e) {
    stop("Failed to generate voice-overs: ", e$message)
  })

  # --- Generate Images ---
  wrap_text <- function(text, width = 35) {
    words <- unlist(strsplit(text, " "))
    lines <- character()
    current_line <- ""
    for (word in words) {
      if (nchar(current_line) + nchar(word) + 1 <= width) {
        current_line <- if (nchar(current_line) == 0) word else paste(current_line, word)
      } else {
        lines <- c(lines, current_line)
        current_line <- word
      }
    }
    if (nchar(current_line) > 0) lines <- c(lines, current_line)
    paste(lines, collapse = "\n")
  }

  create_quote_image <- function(quote, author, output_file) {
    # Load background image dimensions and data
    if (requireNamespace("jpeg", quietly = TRUE)) {
      bg <- tryCatch({
        jpeg::readJPEG(background_path)
      }, error = function(e) {
        message("Failed to load background.jpg with jpeg: ", e$message)
        NULL
      })
      if (is.null(bg)) {
        if (requireNamespace("png", quietly = TRUE)) {
          bg <- tryCatch({
            png::readPNG(background_path)
          }, error = function(e) {
            message("Failed to load background.jpg with png: ", e$message)
            NULL
          })
        }
      }
    } else if (requireNamespace("png", quietly = TRUE)) {
      bg <- tryCatch({
        png::readPNG(background_path)
      }, error = function(e) {
        message("Failed to load background.jpg with png: ", e$message)
        NULL
      })
    } else {
      stop("Neither jpeg nor png package available. Install one of them.")
    }

    if (is.null(bg)) {
      stop("Failed to load background.jpg. Ensure it’s a valid JPG or PNG file.")
    }

    # Get background dimensions
    if (length(dim(bg)) == 3) {  # RGB image
      width <- dim(bg)[2]
      height <- dim(bg)[1]
    } else {  # Grayscale or other format
      width <- dim(bg)[1]
      height <- dim(bg)[2]
    }

    # Open PNG device with background image dimensions
    png(output_file, width = width, height = height, units = "px", bg = "transparent")

    # Plot background image
    plot.new()
    par(mar = c(0, 0, 0, 0))
    plot.window(xlim = c(0, width), ylim = c(0, height), asp = 1)
    tryCatch({
      rasterImage(bg, 0, 0, width, height)
    }, error = function(e) {
      message("Failed to render background image: ", e$message, ". Using solid background.")
      rect(0, 0, width, height, col = "#333333", border = NA)  # Dark gray fallback
    })

    # Wrap and add quote text without shades
    wrapped_quote <- wrap_text(quote, width = 35)
    full_text <- paste0("\"", wrapped_quote, "\"\n\n- ", author)

    # Add text
    text_x <- width / 2
    text_y <- height / 2 + 200  # Offset from center
    text_cex <- 3  # Adjust text size

    text(text_x, text_y, full_text, col = "white", cex = text_cex, font = 2)

    # Close device
    dev.off()
    message("Created image: ", output_file)
  }

  message("Generating quote images...")
  tryCatch({
    pmap(list(quotes$Quote, quotes$Author, quotes$image_file), create_quote_image)
  }, error = function(e) {
    stop("Failed to generate images: ", e$message)
  })

  # --- Generate Videos ---
  create_quote_video <- function(image_file, voiceover_file, video_file) {
    if (!file.exists(image_file)) stop(paste("Image file not found:", image_file))
    if (!file.exists(voiceover_file)) stop(paste("Voice-over file not found:", voiceover_file))
    voiceover_file_escaped <- shQuote(voiceover_file)
    image_file_escaped <- shQuote(image_file)
    video_file_escaped <- shQuote(video_file)
    duration_cmd <- paste("ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1", voiceover_file_escaped)
    duration_output <- system(duration_cmd, intern = TRUE, ignore.stderr = TRUE)
    duration <- as.numeric(duration_output)
    if (is.na(duration)) {
      stop("Could not determine voice-over duration. ffprobe output: ", paste(duration_output, collapse = ", "))
    }
    ffmpeg_cmd <- paste0(
      "ffmpeg -loop 1 -i ", image_file_escaped,
      " -i ", voiceover_file_escaped,
      " -c:v libx264 -t ", duration,
      " -pix_fmt yuv420p -vf scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2 ",
      video_file_escaped,
      " -y"
    )
    status <- system(ffmpeg_cmd, intern = FALSE, ignore.stderr = FALSE)
    if (status != 0) {
      stop("FFmpeg command failed with status ", status, ": ", ffmpeg_cmd)
    }
    message("Created video: ", video_file)
  }

  message("Generating quote videos...")
  tryCatch({
    pmap(list(quotes$image_file, quotes$voiceover_file, quotes$video_file), create_quote_video)
  }, error = function(e) {
    stop("Failed to generate videos: ", e$message)
  })

  # --- Combine Videos with Adjusted Audio Speed and Pauses ---
  combine_videos <- function(video_files, music_file, output_file, pause_duration = 4) {
    if (!all(file.exists(video_files))) stop("One or more video files not found: ", paste(video_files[!file.exists(video_files)], collapse = ", "))
    if (!file.exists(music_file)) stop(paste("Music file not found:", music_file))

    # Create temporary extended video files with pauses
    extended_video_files <- character()
    for (i in seq_along(video_files)) {
      input_file <- video_files[i]
      output_file_temp <- paste0("temp_extended_", basename(input_file))
      extended_video_files <- c(extended_video_files, output_file_temp)

      extend_cmd <- paste0(
        "ffmpeg -i ", shQuote(input_file),
        " -filter_complex \"[0:v]tpad=stop_mode=clone:stop_duration=", pause_duration, "[v];[0:a]apad=pad_dur=", pause_duration, "[a]\" ",
        " -map \"[v]\" -map \"[a]\" -c:v libx264 -c:a aac -shortest ",
        shQuote(output_file_temp),
        " -y"
      )
      message("Running extend command: ", extend_cmd)
      extend_output <- system(extend_cmd, intern = TRUE, ignore.stderr = FALSE)
      extend_status <- attr(extend_output, "status") %||% 0
      if (extend_status != 0) {
        stop("FFmpeg extend command failed with status ", extend_status, ":\n", paste(extend_output, collapse = "\n"))
      }
      message("Extended video created: ", output_file_temp)
    }

    # Prepare the concatenation list
    video_list <- tempfile(fileext = ".txt")
    writeLines(paste0("file '", normalizePath(extended_video_files, winslash = "/"), "'"), video_list)
    message("Concatenation list contents:\n", paste(readLines(video_list), collapse = "\n"))

    # Escape file paths
    music_file_escaped <- shQuote(music_file)
    output_file_escaped <- shQuote(output_file)

    # FFmpeg command with audio speed adjustment and mixing
    ffmpeg_cmd <- paste0(
      "ffmpeg -f concat -safe 0 -i ", shQuote(video_list),
      " -i ", music_file_escaped,
      " -filter_complex \"[0:v]setpts=1.111*PTS[v];[0:a]atempo=0.9[a0];[1:a]volume=0.3[a1];[a0][a1]amix=inputs=2:duration=shortest[aout]\" ",
      " -map \"[v]\" -map \"[aout]\" -c:v libx264 -c:a aac -shortest ",
      output_file_escaped,
      " -y"
    )
    message("Running FFmpeg concat command: ", ffmpeg_cmd)
    ffmpeg_output <- system(ffmpeg_cmd, intern = TRUE, ignore.stderr = FALSE)
    ffmpeg_status <- attr(ffmpeg_output, "status") %||% 0
    if (ffmpeg_status != 0) {
      stop("FFmpeg concat command failed with status ", ffmpeg_status, ":\n", paste(ffmpeg_output, collapse = "\n"))
    }

    # Clean up
    unlink(video_list)
    unlink(extended_video_files)

    if (!file.exists(output_file)) {
      stop("Failed to create output file: ", output_file)
    }
    message("Created combined video: ", output_file)
  }

  # Define input and output files (relative to project_dir)
  video_files <- quotes$video_file
  music_file <- file.path("..", "music.mp3")
  output_file <- "final_video.mp4"

  # Ensure music file exists
  if (!file.exists(music_file)) {
    message("Music file '", music_file, "' not found. Please download a royalty-free MP3 from uppbeat.io and place it in the parent directory.")
    stop("Missing music file")
  }

  # Generate combined video
  message("Combining videos and adding music...")
  tryCatch({
    combine_videos(video_files, music_file, output_file, pause_duration = 4)
  }, error = function(e) {
    stop("Failed to combine videos: ", e$message)
  })

  message("Video processing complete. Check '", output_file, "' in ", project_dir)

  # Reset working directory
  setwd(initial_wd)

  invisible(NULL)
}
