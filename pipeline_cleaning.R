.libPaths(c('/usr/local/lib/R/site-library', .libPaths()))

# Load all packages
library(tidyverse)
library(randomForest)
library(caret)
library(Metrics)
library(data.table)
library(ggplot2)
library(googledrive)

drive_auth(path = Sys.getenv("GDRIVE_KEY_PATH"))


combine_navs_csvs <- function(folder_path = "Navs CSVs", use_google_drive = TRUE) {

  cat("========================================\n")
  cat("Combining CSVs\n")
  cat("========================================\n\n")

  if (use_google_drive) {

    cat("Searching for folder:", folder_path, "\n")
    folder <- drive_get(folder_path)

    if (nrow(folder) == 0) {
      stop("Folder not found")
    }

    cat("Finding CSVs in folder...\n")
    files <- drive_ls(folder, pattern = "\\.csv$")

    if (nrow(files) == 0) {
      stop("No CSV files found in the folder")
    }

    cat("Found", nrow(files), "CSV files\n\n")

    temp_dir <- tempdir()

    all_data_list <- list()
    for (i in 1:nrow(files)) {
      cat("Processing file", i, "of", nrow(files), ":", files$name[i], "\n")

      temp_file <- file.path(temp_dir, files$name[i])
      tryCatch({
        drive_download(files$id[i], path = temp_file, overwrite = TRUE, verbose = FALSE)

        df <- data.table::fread(temp_file,
                                stringsAsFactors = FALSE,
                                na.strings = c("", "NA", "N/A", "null", "NULL"),
                                fill = TRUE)

        df$SourceFile <- files$name[i]
        all_data_list[[i]] <- df
        cat(" Loaded", nrow(df), "rows\n")
      }, error = function(e) {
        cat(" Error loading file:", e$message, "\n")
      })

      if (file.exists(temp_file)) {
        unlink(temp_file)
      }
    }

  } else {
    stop("Please use Google Drive")
  }

  all_data_list <- all_data_list[!sapply(all_data_list, is.null)]

  if (length(all_data_list) == 0) {
    stop("No data")
  }

  cat("\nCombining", length(all_data_list), "dataframes...\n")

  combined_data <- data.table::rbindlist(all_data_list,
                                          use.names = TRUE,
                                          fill = TRUE,
                                          idcol = "FileIndex")
  combined_data <- as.data.frame(combined_data)

  cat("Successfully combined data\n")
  cat("Total rows:", nrow(combined_data), "\n")
  cat("Total columns:", ncol(combined_data), "\n\n")

  return(combined_data)
}


clean_all_data <- function(df) {

  cat("Cleaning Data \n")
  original_rows <- nrow(df)

  if (all(c("PitcherId", "Pitcher") %in% names(df))) {
    pitcher_mapping <- df %>%
      select(PitcherId, Pitcher) %>%
      distinct() %>%
      filter(!is.na(Pitcher), !is.na(PitcherId))

    cat("Created pitcher ID to name mapping:\n")
    cat(sprintf(" Found %d unique pitcher IDs with names\n", nrow(pitcher_mapping)))
    attr(df, "pitcher_mapping") <- pitcher_mapping
  }

  if (all(c("PitchCall", "PlayResult") %in% names(df))) {
    cat("\nStep 1: Updating PitchCall for InPlay events:\n")

    inplay_count <- sum(df$PitchCall == "InPlay", na.rm = TRUE)
    cat(sprintf(" Total InPlay events: %d\n", inplay_count))

    homerun_count <- sum(df$PlayResult == "Homerun", na.rm = TRUE)
    sinigle_count <- sum(df$PlayResult == "Sinigle", na.rm = TRUE)

    df <- df %>%
      mutate(
        PlayResult = case_when(
          PlayResult == "Homerun" ~ "HomeRun",
          PlayResult == "Sinigle" ~ "Single",
          TRUE ~ PlayResult
        )
      )

    if (homerun_count > 0) cat(sprintf(" Standardized Homerun to HomeRun: %d entries\n", homerun_count))
    if (sinigle_count > 0) cat(sprintf(" Fixed Sinigle to Single: %d entries\n", sinigle_count))

    inplay_indices <- which(df$PitchCall == "InPlay")
    if (length(inplay_indices) > 0) {
      df$PitchCall[inplay_indices] <- df$PlayResult[inplay_indices]
      na_inplay <- inplay_indices[is.na(df$PitchCall[inplay_indices]) | df$PitchCall[inplay_indices] == ""]
      if (length(na_inplay) > 0) {
        df$PitchCall[na_inplay] <- "InPlay"
      }
    }
  }

  if ("PitchCall" %in% names(df)) {
    cat("\nStep 2: Cleaning PitchCall column:\n")

    df <- df %>%
      mutate(
        PitchCall = case_when(
          PitchCall == "BallIntentional"       ~ "BallCalled",
          PitchCall == "BallInDirt"            ~ "BallCalled",
          PitchCall == "BallinDirt"            ~ "BallCalled",
          PitchCall == "Ball In Dirt"          ~ "BallCalled",
          PitchCall == "FoulBallNotFieldable"  ~ "FoulBall",
          PitchCall == "FoulBallFieldable"     ~ "FoulBall",
          PitchCall == "Homerun"               ~ "HomeRun",
          PitchCall == "Sinigle"               ~ "Single",
          TRUE ~ PitchCall
        )
      )

    remaining_ball_in_dirt <- sum(df$PitchCall == "BallInDirt", na.rm = TRUE)
    if (remaining_ball_in_dirt > 0) {
      df$PitchCall[df$PitchCall == "BallInDirt"] <- "BallCalled"
    }
  }

  if ("PlayResult" %in% names(df)) {
    cat("\nStep 3: Cleaning PlayResult column:\n")

    df <- df %>%
      mutate(
        PlayResult = case_when(
          PlayResult == "StolenBase"     ~ "Undefined",
          PlayResult == "CaughtStealing" ~ "Undefined",
          PlayResult == "Sinigle"        ~ "Single",
          PlayResult == "Homerun"        ~ "HomeRun",
          TRUE ~ PlayResult
        )
      )
  }

  if (all(c("TaggedPitchType", "AutoPitchType") %in% names(df))) {
    cat("\nStep 4: Cleaning pitch types:\n")

    df <- df %>% filter(TaggedPitchType != "Other")

    df$TaggedPitchType_Original <- df$TaggedPitchType

    df <- df %>%
      mutate(
        TaggedPitchType = case_when(
          TaggedPitchType == "Undefined"        ~ AutoPitchType,
          is.na(TaggedPitchType)                ~ AutoPitchType,
          TaggedPitchType == ""                 ~ AutoPitchType,
          TaggedPitchType == "TwoSeamFastBall"  ~ "Sinker",
          TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
          TaggedPitchType == "Four-Seam"        ~ "Fastball",
          TaggedPitchType == "ChangeUp"         ~ "Changeup",
          TRUE ~ TaggedPitchType
        )
      )

    df <- df %>% filter(TaggedPitchType != "Other")
  }

  if (all(c("TaggedPitchType", "RelSpeed") %in% names(df))) {
    cat("\nStep 5: Filtering velocity outliers:\n")
    df <- df %>% filter(!(TaggedPitchType == "Fastball" & RelSpeed < 77))
  }

  cat(sprintf("\nData cleaning complete. Final row count: %d (removed %d rows)\n",
              nrow(df), original_rows - nrow(df)))
  return(df)
}


filter_core_metrics <- function(df) {

  cat("\nFiltering for velo, spin, IVB, and HB\n")
  original_rows <- nrow(df)

  df_filtered <- df %>%
    filter(!is.na(RelSpeed),
           !is.na(SpinRate),
           !is.na(InducedVertBreak),
           !is.na(HorzBreak))

  rows_removed <- original_rows - nrow(df_filtered)
  cat(sprintf(" Rows retained: %d (removed %d)\n", nrow(df_filtered), rows_removed))
  return(df_filtered)
}

assess_final_quality <- function(df) {
  cat("Dataset Overview:\n")
  cat(sprintf(" Total rows: %d\n", nrow(df)))
  cat(sprintf(" Total columns: %d\n", ncol(df)))
  if ("PitcherId" %in% names(df)) cat(sprintf(" Unique pitchers: %d\n", n_distinct(df$PitcherId)))
  return(TRUE)
}


run_necbl_pipeline <- function(folder_path = "Navs CSVs", save_to_file = TRUE) {

  cat("STEP 1: Combining all CSV files\n")
  combined_data <- combine_navs_csvs(folder_path, use_google_drive = TRUE)
  initial_rows <- nrow(combined_data)
  initial_cols <- ncol(combined_data)

  cat("\nSTEP 2: Comprehensive data cleaning\n")
  combined_data <- clean_all_data(combined_data)

  cat("\nSTEP 3: Filter for complete core metrics\n")
  combined_data <- filter_core_metrics(combined_data)

  cat("\nSTEP 4: Final quality assessment\n")
  assess_final_quality(combined_data)

  if (save_to_file) {
    rds_file <- paste0("necbl_clean_", Sys.Date(), ".rds")
    saveRDS(combined_data, rds_file, compress = TRUE)
    cat("Saved:", rds_file, "\n")

    pitcher_mapping <- attr(combined_data, "pitcher_mapping")
    if (!is.null(pitcher_mapping)) {
      mapping_file <- paste0("pitcher_id_mapping_", Sys.Date(), ".csv")
      write.csv(pitcher_mapping, mapping_file, row.names = FALSE)
    }
  }

  cat(sprintf("\nTransformation: %d rows to %d rows (%.1f%% retained)\n",
              initial_rows, nrow(combined_data),
              nrow(combined_data) / initial_rows * 100))

  necbl_clean_data <<- combined_data
  return(combined_data)
}


necbl_data <- run_necbl_pipeline(
  folder_path  = "Navs CSVs",
  save_to_file = TRUE
)
