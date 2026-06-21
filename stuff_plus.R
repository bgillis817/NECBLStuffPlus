library(tidyverse)
library(randomForest)
library(caret)
library(ggplot2)
library(bit64)

set.seed(42)

prepare_stuff_data <- function(df) {

  cat("NECBL Stuff+ Data Preparation \n")
  cat("==============================================\n")

  if (class(df$PitcherId)[1] == "integer64") {
    cat("Converting PitcherId from integer64 to character...\n")
    df$PitcherId <- as.character(df$PitcherId)
  }

  if ("Date" %in% names(df)) {
    df <- df %>%
      mutate(
        Date   = as.Date(Date, format = "%m/%d/%Y"),
        Season = as.character(format(Date, "%Y"))
      )
    cat("Seasons found in data:", paste(unique(df$Season), collapse = ", "), "\n")
  } else {
    # ── CHANGED: updated fallback season from 2025 to 2026 ──────────────────
    df$Season <- "2026"
    # ────────────────────────────────────────────────────────────────────────
    cat("No Date column found - defaulting to 2026 season\n")
  }

  pitcher_name_map <- df %>%
    select(PitcherId, Pitcher, PitcherTeam, Season) %>%
    distinct() %>%
    filter(!is.na(Pitcher), Pitcher != "")

  cat(sprintf("Preserved %d pitcher-team-season combinations\n", nrow(pitcher_name_map)))

  if ("PitcherTeam" %in% names(df)) {
    teams <- unique(df$PitcherTeam)
    cat("Teams found:", paste(teams[!is.na(teams)], collapse = ", "), "\n")
  }

  df <- df %>%
    filter(
      !is.na(TaggedPitchType),
      TaggedPitchType != "Other",
      TaggedPitchType != "Undefined",
      PitchCall != "Undefined",
      PitchCall != "BallIntentional",
      !is.na(SpinRate),
      !is.na(RelSpeed),
      !is.na(InducedVertBreak),
      !is.na(HorzBreak),
      !is.na(Extension),
      !is.na(RelHeight),
      !is.na(RelSide)
    )

  df <- df %>%
    mutate(
      PitchCall = case_when(
        PitchCall == "Homerun" ~ "HomeRun",
        PitchCall == "Sinigle" ~ "Single",
        TRUE ~ PitchCall
      ),
      PitchCall = case_when(
        PitchCall == "InPlay" & PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ PlayResult,
        PitchCall == "InPlay" ~ "Out",
        PitchCall %in% c("StrikeCalled", "StikeCalled", "Strikecalled") ~ "StrikeCalled",
        PitchCall %in% c("BallInDirt", "BallinDirt", "BallIntentional") ~ "BallCalled",
        PitchCall %in% c("FoulBall", "FoulBallNotFieldable", "FoulBallFieldable") ~ "Foul",
        PitchCall == "FieldersChoice" ~ "Out",
        PitchCall == "Sacrifice" ~ "Out",
        TRUE ~ PitchCall
      ),
      PitcherThrows = ifelse(PitcherThrows == "RIght", "Right", PitcherThrows),
      BatterSide    = ifelse(BatterSide    == "RIght", "Right", BatterSide)
    ) %>%
    filter(
      PitcherThrows %in% c("Left", "Right"),
      BatterSide    %in% c("Left", "Right")
    )

  if (!"Balls"   %in% names(df)) { df$Balls   <- 0; cat("Created Balls column with default value 0\n") }
  if (!"Strikes" %in% names(df)) { df$Strikes <- 0; cat("Created Strikes column with default value 0\n") }

  df <- df %>%
    mutate(
      Balls           = factor(pmin(as.numeric(as.character(Balls)),   3), levels = c(0,1,2,3), ordered = TRUE),
      Strikes         = factor(pmin(as.numeric(as.character(Strikes)), 2), levels = c(0,1,2),   ordered = TRUE),
      PitchCall       = factor(PitchCall),
      PitcherThrows   = factor(PitcherThrows, levels = c("Left", "Right")),
      BatterSide      = factor(BatterSide,    levels = c("Left", "Right")),
      TaggedPitchType = as.factor(TaggedPitchType)
    )

  attr(df, "pitcher_names") <- pitcher_name_map
  cat(sprintf("Cleaned data: %d pitches\n", nrow(df)))
  return(df)
}


build_stuff_plus_model <- function(df) {

  cat("\n\nBuilding NECBL Stuff+ Model \n")
  cat("===================================================\n\n")

  pitcher_names <- attr(df, "pitcher_names")

  set.seed(2425)
  train_index <- createDataPartition(df$PitchCall, p = 0.7, list = FALSE)
  train <- df[train_index, ]
  vali  <- df[-train_index, ]

  cat(sprintf("Training set (for model building): %d pitches\n", nrow(train)))
  cat(sprintf("Validation set (for model evaluation): %d pitches\n\n", nrow(vali)))

  model_formula <- as.formula("PitchCall ~ RelSpeed + SpinRate + InducedVertBreak + HorzBreak + Extension + RelHeight + RelSide")

  if (all(c("VertApprAngle", "HorzApprAngle") %in% names(df))) {
    model_formula <- as.formula("PitchCall ~ RelSpeed + SpinRate + InducedVertBreak + HorzBreak + Extension + RelHeight + RelSide + VertApprAngle + HorzApprAngle")
    cat("Including approach angles in model\n")
  }

  rf_model <- randomForest(
    model_formula,
    data       = train,
    ntree      = 250,
    mtry       = floor(sqrt(length(all.vars(model_formula)) - 1)),
    importance = TRUE
  )

  print(rf_model)

  rf_pred_vali <- predict(rf_model, newdata = vali)
  conf_matrix  <- confusionMatrix(rf_pred_vali, vali$PitchCall)
  cat("\nValidation Set Accuracy:", round(conf_matrix$overall["Accuracy"], 4), "\n")

  cat("\nApplying model to entire dataset...\n")
  rf_prob_all <- predict(rf_model, newdata = df, type = "prob")
  cat(sprintf("Scored all %d pitches in dataset\n", nrow(df)))

  return(list(
    model         = rf_model,
    full_data     = df,
    full_prob     = rf_prob_all,
    pitcher_names = pitcher_names
  ))
}


calculate_run_values <- function(prob_df, strikes) {

  weights <- list(
    ball_01   =  0.056,
    strike_01 = -0.089,
    foul_01   = -0.089,
    ball_2    =  0.056,
    strike_2  = -0.089,
    foul_2    =  0,
    out       = -0.26,
    single    =  0.44,
    double    =  0.75,
    triple    =  1.01,
    homerun   =  1.40,
    hbp       =  0.31
  )

  n <- nrow(prob_df)
  xRunValue <- numeric(n)

  for (i in 1:n) {
    s <- as.numeric(as.character(strikes[i]))
    if (is.na(s)) s <- 0

    if (s < 2) {
      xRunValue[i] <-
        ifelse("BallCalled"     %in% names(prob_df), prob_df$BallCalled[i]     * weights$ball_01,   0) +
        ifelse("StrikeCalled"   %in% names(prob_df), prob_df$StrikeCalled[i]   * weights$strike_01, 0) +
        ifelse("StrikeSwinging" %in% names(prob_df), prob_df$StrikeSwinging[i] * weights$strike_01, 0) +
        ifelse("Foul"           %in% names(prob_df), prob_df$Foul[i]           * weights$foul_01,   0) +
        ifelse("Out"            %in% names(prob_df), prob_df$Out[i]            * weights$out,       0) +
        ifelse("Single"         %in% names(prob_df), prob_df$Single[i]         * weights$single,    0) +
        ifelse("Double"         %in% names(prob_df), prob_df$Double[i]         * weights$double,    0) +
        ifelse("Triple"         %in% names(prob_df), prob_df$Triple[i]         * weights$triple,    0) +
        ifelse("HomeRun"        %in% names(prob_df), prob_df$HomeRun[i]        * weights$homerun,   0) +
        ifelse("HitByPitch"     %in% names(prob_df), prob_df$HitByPitch[i]     * weights$hbp,       0)
    } else {
      xRunValue[i] <-
        ifelse("BallCalled"     %in% names(prob_df), prob_df$BallCalled[i]     * weights$ball_2,    0) +
        ifelse("StrikeCalled"   %in% names(prob_df), prob_df$StrikeCalled[i]   * weights$strike_2,  0) +
        ifelse("StrikeSwinging" %in% names(prob_df), prob_df$StrikeSwinging[i] * weights$strike_2,  0) +
        ifelse("Foul"           %in% names(prob_df), prob_df$Foul[i]           * weights$foul_2,    0) +
        ifelse("Out"            %in% names(prob_df), prob_df$Out[i]            * weights$out,       0) +
        ifelse("Single"         %in% names(prob_df), prob_df$Single[i]         * weights$single,    0) +
        ifelse("Double"         %in% names(prob_df), prob_df$Double[i]         * weights$double,    0) +
        ifelse("Triple"         %in% names(prob_df), prob_df$Triple[i]         * weights$triple,    0) +
        ifelse("HomeRun"        %in% names(prob_df), prob_df$HomeRun[i]        * weights$homerun,   0) +
        ifelse("HitByPitch"     %in% names(prob_df), prob_df$HitByPitch[i]     * weights$hbp,       0)
    }
  }
  return(xRunValue)
}


calculate_stuff_plus <- function(model_results) {

  cat("\n\nCalculating Stuff+ Scores\n")
  cat("========================================\n\n")

  full_data     <- model_results$full_data
  rf_prob       <- as.data.frame(model_results$full_prob)
  pitcher_names <- model_results$pitcher_names

  full_data$PitcherId     <- as.character(full_data$PitcherId)
  pitcher_names$PitcherId <- as.character(pitcher_names$PitcherId)

  full_data <- full_data %>%
    mutate(
      ActualRunValue = case_when(
        Strikes %in% c(0, 1) & PitchCall == "BallCalled"                                    ~  0.056,
        Strikes %in% c(0, 1) & PitchCall %in% c("StrikeCalled", "StrikeSwinging", "Foul")  ~ -0.089,
        Strikes == 2          & PitchCall == "BallCalled"                                    ~  0.056,
        Strikes == 2          & PitchCall %in% c("StrikeCalled", "StrikeSwinging")          ~ -0.089,
        Strikes == 2          & PitchCall == "Foul"                                          ~  0,
        PitchCall == "Out"        ~ -0.26,
        PitchCall == "Single"     ~  0.44,
        PitchCall == "Double"     ~  0.75,
        PitchCall == "Triple"     ~  1.01,
        PitchCall == "HomeRun"    ~  1.40,
        PitchCall == "HitByPitch" ~  0.31,
        TRUE ~ 0
      )
    )

  full_data$xRunValue <- calculate_run_values(rf_prob, full_data$Strikes)

  pitcher_summary <- full_data %>%
    mutate(PitcherId = as.character(PitcherId)) %>%
    group_by(PitcherId, PitcherTeam, Season) %>%
    summarise(
      n_pitches       = n(),
      avg_actual_rv   = mean(ActualRunValue, na.rm = TRUE),
      avg_expected_rv = mean(xRunValue, na.rm = TRUE),
      .groups = "drop"
    )

  league_mean <- mean(pitcher_summary$avg_expected_rv)
  league_sd   <- sd(pitcher_summary$avg_expected_rv)

  pitcher_summary <- pitcher_summary %>%
    mutate(
      z_score    = (avg_expected_rv - league_mean) / league_sd,
      stuff_plus = round(100 - (z_score * 10), 1),
      percentile = round(percent_rank(stuff_plus) * 100, 1)
    ) %>%
    left_join(pitcher_names, by = c("PitcherId", "PitcherTeam", "Season")) %>%
    select(PitcherId, Pitcher, PitcherTeam, Season, n_pitches, stuff_plus, percentile) %>%
    arrange(desc(stuff_plus))

  cat(sprintf("Mean: %.1f  SD: %.1f\n", mean(pitcher_summary$stuff_plus), sd(pitcher_summary$stuff_plus)))
  return(pitcher_summary)
}


calculate_stuff_plus_by_pitch <- function(model_results) {

  cat("\n\nCalculating Stuff+ by Pitch Type\n")
  cat("===============================================\n\n")

  full_data     <- model_results$full_data
  rf_prob       <- as.data.frame(model_results$full_prob)
  pitcher_names <- model_results$pitcher_names

  full_data$PitcherId     <- as.character(full_data$PitcherId)
  pitcher_names$PitcherId <- as.character(pitcher_names$PitcherId)

  full_data <- full_data %>%
    mutate(
      ActualRunValue = case_when(
        Strikes %in% c(0, 1) & PitchCall == "BallCalled"                                    ~  0.056,
        Strikes %in% c(0, 1) & PitchCall %in% c("StrikeCalled", "StrikeSwinging", "Foul")  ~ -0.089,
        Strikes == 2          & PitchCall == "BallCalled"                                    ~  0.056,
        Strikes == 2          & PitchCall %in% c("StrikeCalled", "StrikeSwinging")          ~ -0.089,
        Strikes == 2          & PitchCall == "Foul"                                          ~  0,
        PitchCall == "Out"        ~ -0.26,
        PitchCall == "Single"     ~  0.44,
        PitchCall == "Double"     ~  0.75,
        PitchCall == "Triple"     ~  1.01,
        PitchCall == "HomeRun"    ~  1.40,
        PitchCall == "HitByPitch" ~  0.31,
        TRUE ~ 0
      )
    )

  full_data$xRunValue <- calculate_run_values(rf_prob, full_data$Strikes)

  overall_league_stats <- full_data %>%
    group_by(PitcherId, PitcherTeam, Season) %>%
    summarise(avg_expected_rv = mean(xRunValue, na.rm = TRUE), .groups = "drop")

  league_mean <- mean(overall_league_stats$avg_expected_rv)
  league_sd   <- sd(overall_league_stats$avg_expected_rv)

  cat(sprintf("Using unified league baseline: Mean = %.4f, SD = %.4f\n", league_mean, league_sd))

  pitch_summary <- full_data %>%
    mutate(PitcherId = as.character(full_data$PitcherId)) %>%
    group_by(PitcherId, TaggedPitchType, PitcherTeam, Season) %>%
    summarise(
      n_pitches       = n(),
      avg_actual_rv   = mean(ActualRunValue, na.rm = TRUE),
      avg_expected_rv = mean(xRunValue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      z_score    = (avg_expected_rv - league_mean) / league_sd,
      stuff_plus = round(100 - (z_score * 10), 1),
      percentile = round(percent_rank(stuff_plus) * 100, 1)
    ) %>%
    select(-z_score, -avg_actual_rv, -avg_expected_rv) %>%
    left_join(pitcher_names, by = c("PitcherId", "PitcherTeam", "Season")) %>%
    select(PitcherId, Pitcher, PitcherTeam, TaggedPitchType, Season, n_pitches, stuff_plus, percentile) %>%
    arrange(Pitcher, TaggedPitchType)

  return(pitch_summary)
}


run_necbl_stuff_plus_no_threshold <- function() {

  cat("NECBL Stuff+ Pipeline\n")
  cat("=====================================================\n")

  # ── CHANGED (1 of 2) ──────────────────────────────────────────────────────
  # Original: df <- readRDS("necbl_clean_2025-09-02.rds")
  rds_files <- list.files(pattern = "^necbl_clean_.*\\.rds$")
  if (length(rds_files) == 0) stop("No necbl_clean_*.rds file found. Run pipeline_cleaning.R first.")
  latest_rds <- tail(sort(rds_files), 1)
  cat("Loading data from:", latest_rds, "\n")
  df <- readRDS(latest_rds)
  # ──────────────────────────────────────────────────────────────────────────

  df              <- prepare_stuff_data(df)
  model_results   <- build_stuff_plus_model(df)
  overall_summary <- calculate_stuff_plus(model_results)
  pitch_summary   <- calculate_stuff_plus_by_pitch(model_results)

  # ── CHANGED (2 of 2) ──────────────────────────────────────────────────────
  # Original: saveRDS(overall_summary, "necbl_stuff_plus_overall_2025-09-05.rds")
  #           saveRDS(pitch_summary,   "necbl_stuff_plus_by_pitch_type_2025-09-05.rds")
  saveRDS(overall_summary, paste0("necbl_stuff_plus_overall_",       Sys.Date(), ".rds"))
  saveRDS(pitch_summary,   paste0("necbl_stuff_plus_by_pitch_type_", Sys.Date(), ".rds"))
  # ──────────────────────────────────────────────────────────────────────────

  cat("\n\nPipeline complete! Results saved.\n")
  cat(sprintf(" - necbl_stuff_plus_overall_%s.rds\n",       Sys.Date()))
  cat(sprintf(" - necbl_stuff_plus_by_pitch_type_%s.rds\n", Sys.Date()))

  cat("\nTop 10 Overall:\n")
  print(head(overall_summary %>% arrange(desc(stuff_plus)), 10))

  return(list(overall = overall_summary, by_pitch = pitch_summary, model = model_results$model))
}


results <- run_necbl_stuff_plus_no_threshold()
