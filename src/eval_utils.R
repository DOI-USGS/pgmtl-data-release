#### GLM-Specific versions of matching obs and computing RMSE ####

# filter observations to those sites with a specified minimum number of observed dates
filter_min_dates <- function(obs_rds_file, min_dates){

  obs <- readRDS(obs_rds_file)
  eval_sites <- group_by(obs, site_id) %>% summarize(n_dates = length(unique(date))) %>% filter(n_dates >= min_dates) %>%
    pull(site_id)

  filter(obs, site_id %in% eval_sites)
}

# interpolate the predictions to the depths of the observations
match_glm_obs <- function(target_name, eval_data, predict_df){

  file_info <- predict_df %>% select(site_id, source_filepath) 
  purrr::map(1:nrow(file_info), function(x){
    this_file <- file_info$source_filepath[x]
    this_id <- file_info$site_id[x]
    these_obs <- eval_data %>% filter(site_id %in% this_id) %>% 
      rename(obs = temp)
    
    feather::read_feather(this_file) %>% 
      mutate(time = as.Date(lubridate::floor_date(DateTime, 'days'))) %>% select(time, contains('temp_')) %>%
      pivot_longer(-time, names_to = 'depth', values_to = 'temp', names_prefix = 'temp_') %>%
      mutate(depth = as.numeric(depth)) %>% filter(time %in% these_obs$date) %>%
      rename(date = time, pred = temp) %>% 
      right_join(these_obs, by = c("date", "depth")) %>% 
      select(site_id, date, depth, obs, pred, source) %>% 
      filter(!is.na(pred))
  }) %>% purrr::reduce(bind_rows)
}

match_extend_glm_obs <- function(target_name, eval_data, predict_df){
  
  file_info <- predict_df %>% select(site_id, source_filepath) 
  
  purrr::map(1:nrow(file_info), function(x){
    this_file <- file_info$source_filepath[x]
    this_id <- file_info$site_id[x]
    these_obs <- eval_data %>% filter(site_id %in% this_id) %>% 
      rename(obs = temp)
    
    # if max(depth) of obs is greater than max of data.frame, fill w/ tidyr
    model_preds <- feather::read_feather(this_file) %>% 
      mutate(time = as.Date(lubridate::floor_date(DateTime, 'days'))) %>% select(time, contains('temp_'))
    
    z_max_pred <- tail(names(model_preds), 1) %>% {strsplit(., '_')[[1]][2]} %>% as.numeric()
    z_max_obs <- max(these_obs$depth)
    
    if (z_max_obs > z_max_pred){
      model_preds[[sprintf('temp_%s', z_max_obs)]] <- NA
    }
      
    pivot_longer(model_preds, -time, names_to = 'depth', values_to = 'temp', names_prefix = 'temp_') %>%
      mutate(depth = as.numeric(depth)) %>% filter(time %in% these_obs$date) %>%
      arrange(depth) %>%
      tidyr::fill(temp, .direction = 'down') %>% ungroup() %>%
      rename(date = time, pred = temp) %>% 
      right_join(these_obs, by = c("date", "depth")) %>% 
      select(site_id, date, depth, obs, pred, source) %>% 
      filter(!is.na(pred))
    
  }) %>% purrr::reduce(bind_rows)
  
}

match_pgmtl_obs <- function(target_name, eval_data, predict_df){
  
  file_info <- predict_df %>% select(site_id, source_filepath)
  
  
  purrr::map(1:nrow(file_info), function(x){
    this_file <- file_info$source_filepath[x]
    this_id <- file_info$site_id[x]
    these_obs <- eval_data %>% filter(site_id %in% this_id) %>% 
      rename(obs = temp)
    
    feather::read_feather(this_file) %>% 
      # files are of the form
      # A tibble: 6 x 21
      # index      `0.0` `0.5` `1.0` `1.5` `2.0` `2.5` `3.0` ...
      mutate(date = as.Date(index)) %>% select(-index) %>% 
      pivot_longer(-date, names_to = 'depth', values_to = 'pred') %>% 
      mutate(depth = as.numeric(depth)) %>% 
      filter(date %in% these_obs$date) %>%
      right_join(these_obs, by = c("date", "depth")) %>% 
      select(site_id, date, depth, obs, pred, source) %>% 
      filter(!is.na(pred))
  }) %>% purrr::reduce(bind_rows)
}

# compute RMSEs for each site
compare_as_rmse <- function(target_name, matched_preds){

  mutate(matched_preds, pred_diff = pred-obs) %>%
    group_by(site_id) %>% summarize(rmse = sqrt(mean((pred_diff)^2, na.rm=TRUE))) %>%
    write_csv(path = target_name)
}

#### Model-agnostic functions for assessing predictions ####

# Match model predictions to observation depths and dates
match_preds_to_obs  <- function(out_zip, observations_zip='out_data/temperature_observations.zip', predictions_zips_ind, min_obs_dates){

  # load the observations. only bother assessing those sites with at least
  # min_obs_dates of observations
  obs <- unzip_to_tibble(observations_zip, col_types='cDddc')
  obs_sites <- obs %>%
    group_by(site_id) %>%
    summarize(n_dates = length(unique(date))) %>%
    filter(n_dates >= min_obs_dates) %>%
    pull(site_id)

  # gather the prediction file information. only bother assessing those sites
  # meeting the above observations criteria
  pred_zips <- names(yaml::yaml.load_file(predictions_zips_ind))
  pred_site_info <- bind_rows(lapply(pred_zips, function(pred_zip) {
    tibble(
      source_csv = unzip(pred_zip, list=TRUE)$Name,
      source_zip = pred_zip)
  })) %>%
    extract(source_csv, into='site_id', regex='pgdl_(nhdhr_.+)_test_temperatures\\.csv', remove=FALSE) %>%
    filter(site_id %in% obs_sites)

  # further filter the observations to those sites that have predictions
  obs_eval <- filter(obs, site_id %in% pred_site_info$site_id)

  # interpolate predictions to the depths of the observations
  matched_preds <- purrr::pmap_dfr(pred_site_info, function(source_csv, source_zip, site_id) {
    message('.', appendLF=FALSE)
    these_obs <- obs_eval %>% filter(site_id == !!site_id)
    pred_path <- file.path(tempdir(), source_csv)
    if(!file.exists(pred_path)) {
      unzip(source_zip, exdir = tempdir(), overwrite = TRUE)
    }
    these_preds <- read_preds_release(pred_path)
    prep_pred_obs(these_obs, these_preds)
  })

  zip_this(outfile=out_zip, .object=matched_preds)
}

# Evaluate RMSE of predictions
evaluate_preds <- function(out_file, matched_preds_zip){

  matched_preds <- unzip_to_tibble(matched_preds_zip, col_types='cDddcd')

  matched_preds %>%
    group_by(site_id) %>%
    summarize(rmse = sqrt(mean((pred - obs)^2, na.rm=TRUE))) %>%
    write_csv(path = out_file)
}


#### Function[s] shared by 2+ of the above functions ####

read_preds_release <- function(preds_file) {
  preds <- readr::read_csv(preds_file, col_types = cols(.default = col_double(), date = col_date(format='%Y-%m-%d'))) %>%
    tidyr::gather('temp_depth', 'pred', starts_with('temp')) %>%
    dplyr::mutate(depth = as.numeric(gsub('temp_', '', temp_depth))) %>%
    dplyr::select(date, depth, pred) %>%
    dplyr::arrange(date, depth)
  preds
}

# function to read a test file and identify the corresponding predictions
prep_pred_obs <- function(test_obs, model_preds) {

  # match up preds to test_obs, interpolating predictions to match the observation depths
  pred_obs <- bind_rows(lapply(unique(test_obs$date), function(dt) {
    pred_1d <- filter(model_preds, date == dt, !is.na(depth))
    obs_1d <- filter(test_obs, date == dt) %>%
      rename(obs = temp)
    interp_1d <- tryCatch({
      if(nrow(pred_1d) == 0) stop(sprintf('no predictions on %s', dt))
      if(min(pred_1d$depth) != 0) warning(sprintf('no prediction at 0m on %s', dt))
      mutate(obs_1d, pred = approx(x=pred_1d$depth, y=pred_1d$pred, xout=obs_1d$depth, rule=1)$y)
    }, error=function(e) {
      message(sprintf('approx failed on %s: %s', dt, e$message))
      mutate(obs_1d, pred = NA)
    })
    return(interp_1d)
  }))

  return(pred_obs)
}

