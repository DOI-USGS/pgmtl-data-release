
split_pb_filenames <- function(files_df){
  extract(files_df, file, c('prefix','site_id','suffix'), "(pb0|pball)_(.*)_(temperatures.feather)", remove = FALSE)
}


create_mtl_rmse_table <- function(filepath, pb_mtl_fl, pg_mtl_fl){
  
  pb_mtl <- read_csv(pb_mtl_fl) %>% 
    rename(actual_pb_mtl_rmse = `pb-mtl_rmse`, pred_pb_mtl_rmse = predicted_rmse) %>% 
    group_by(target_id) %>% mutate(
      actual_pb_mtl_rank = rank(actual_pb_mtl_rmse, ties.method = "first"),
      pred_pb_mtl_rank = rank(pred_pb_mtl_rmse, ties.method = "first")) %>% 
    select(target_id, source_id, actual_pb_mtl_rmse, pred_pb_mtl_rmse, actual_pb_mtl_rank, pred_pb_mtl_rank) %>% 
    ungroup()
  
  read_csv(pg_mtl_fl) %>% 
    rename(actual_pgdl_mtl_rmse = `pg-mtl_rmse`, pred_pgdl_mtl_rmse = predicted_rmse) %>% 
    group_by(target_id) %>% mutate(
      actual_pgdl_mtl_rank = rank(actual_pgdl_mtl_rmse, ties.method = "first"),
      pred_pgdl_mtl_rank = rank(pred_pgdl_mtl_rmse, ties.method = "first")) %>% 
    select(target_id, source_id, actual_pgdl_mtl_rmse, pred_pgdl_mtl_rmse, actual_pgdl_mtl_rank, pred_pgdl_mtl_rank) %>% 
    arrange(target_id, actual_pgdl_mtl_rank) %>% 
    ungroup() %>% 
    inner_join(pb_mtl, by = c('target_id','source_id')) %>% 
    write_csv(path = filepath)
}
extract_id_pbmtl <- function(filepath){
  tibble(source_filename = names(yaml::yaml.load_file(filepath))) %>% rowwise() %>% 
    mutate(site_id = {str_split(basename(source_filename), '_t\\|s_')[[1]][1]}) %>% ungroup() %>%
    arrange(site_id) %>% 
    pull(site_id)
}

extract_expansion_ids <- function(filepath, target_ids, source_ids){
  readRDS(filepath) %>% filter(!site_id %in% c(target_ids, source_ids)) %>% 
    pull(site_id) %>% unique() %>% sort()
}

extract_csv_column <- function(filepath, column){
  read_csv(filepath) %>% pull(column) %>% unique() %>% sort()
}

create_release_fl <- function(geometry){
  release_fl <- rep(NA_character_, length(geometry))
  for (i in 1:length(geometry)){
    # format of meteo_nldas_N46.125-46.25_W86.25-86.375.csv
    this_box <- st_bbox(geometry[i])
    release_fl[i] <- sprintf("nldas_meteo_N%1.4f-%1.4f_W%1.4f-%1.4f.csv", this_box$ymin, this_box$ymax, -this_box$xmax, -this_box$xmin)
  }
  return(release_fl)
}
xwalk_meteo_lat_lon <- function(meteo_fl, meteo_dir, ldas_grid){


  all_meteo_fls <- data.frame(pipeline_fl = dir(meteo_dir), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(pipeline_fl, "[0-9n]\\].csv")) %>%
    mutate(x = stringr::str_extract(pipeline_fl, 'x\\[[0-9]+\\]') %>% str_remove('x\\[') %>% str_remove('\\]') %>% as.numeric(),
           y = stringr::str_extract(pipeline_fl, 'y\\[[0-9]+\\]') %>% str_remove('y\\[') %>% str_remove('\\]') %>% as.numeric()) %>%
    left_join(suppressWarnings(st_centroid(ldas_grid))) %>% rename(geometry = ldas_grid_sfc) %>% select(-x, -y) %>%
    st_sf() %>% mutate(release_fl = create_release_fl(geometry))

  meteo_data <- readRDS(meteo_fl) %>% rename(pipeline_fl = meteo_fl)
  left_join(all_meteo_fls, meteo_data, by = 'pipeline_fl') %>% select(site_id, everything()) %>% st_drop_geometry()

}

create_metadata_file <- function(fileout, sites, table, lakes_sf, nml_json_fl, lat_lon_fl, 
                                 meteo_fl_info, gnis_names_fl, meta_fl, target_ids, ext_target_ids, source_ids){
  
  source_type <- tibble(site_id = source_ids, type = 'source')
  target_type <- tibble(site_id = target_ids, type = 'target')
  ext_target_type <- tibble(site_id = ext_target_ids, type = 'ext_target')
  site_types <- rbind(source_type, target_type, ext_target_type)
  sdf <- sf::st_transform(lakes_sf, 2811) %>%
    mutate(perim = lwgeom::st_perimeter_2d(Shape), area = sf::st_area(Shape), circle_perim = 2*pi*sqrt(area/pi), SDF = perim/circle_perim) %>%
    sf::st_drop_geometry() %>% select(site_id, SDF)
  
  mtl_meta <- read_csv(meta_fl) %>% 
    select(site_id = sitse_id, everything(), -glm_uncal_rmse_third, -glm_uncal_rmse_full, -`text name`, -SDF, -latitude, -longitude)
  
  nml_list <- RJSONIO::fromJSON(nml_json_fl)
  
  sites %>% inner_join(mtl_meta, by = 'site_id') %>% 
    inner_join((readRDS(lat_lon_fl)), by = 'site_id') %>%
    inner_join(sdf, by = 'site_id') %>%
    inner_join(site_types, by = 'site_id') %>%
    rename(centroid_lon = longitude, centroid_lat = latitude) %>%
    inner_join(table, by = 'site_id') %>%
    inner_join(meteo_fl_info, by = 'site_id') %>% select(-pipeline_fl) %>%
    inner_join((readRDS(gnis_names_fl)), by = 'site_id') %>%
    select(site_id, lake_name = GNIS_Name, group_id, type, meteo_filename = release_fl, centroid_lon, centroid_lat, state, county, everything()) %>%
    write_csv(fileout)

}
bundle_nml_files <- function(json_filename, xwalk_meteo_fl_names, lake_ids, nml_ind, gnis_names_fl){

  gnis_names <- readRDS(gnis_names_fl)

  prep_proj_dir <- paste(str_split(nml_ind, '/')[[1]][1:2], collapse = '/')
  nml_files <- file.path(prep_proj_dir, names(yaml.load_file(nml_ind)))
  file_bases <- file_bases <- tibble(file = basename(nml_files)) %>% 
    mutate(filebase = str_remove(file, 'pball_|transfer_')) %>% pull(filebase)
  out_list <- vector("list", length = length(lake_ids)) %>% setNames(lake_ids)

  for (id in names(out_list)){
    this_nml_file <- nml_files[file_bases == paste0(id, '.nml')]
    if (!file.exists(this_nml_file)){
      
      stop(this_nml_file, " doesn't exist")
    }
    release_meteo_fl <- filter(xwalk_meteo_fl_names, site_id == id) %>% pull(release_fl)

    lake_name <- filter(gnis_names, site_id == id) %>% pull(GNIS_Name)
    lake_name <- ifelse(is.na(lake_name), 'undefined', lake_name)
    nml <- read_nml(nml_file = this_nml_file) %>%
      set_nml(arg_list = list('meteo_fl' = release_meteo_fl,
                              'lake_name' = lake_name)) %>% unclass()
    out_list[[id]] <- nml
  }

  RJSONIO::toJSON(out_list, pretty = TRUE) %>% write(json_filename)
}

zip_nml_files <- function(zipfile, nml_json){

  cd <- getwd()
  on.exit(setwd(cd))
  zippath <- file.path(getwd(), zipfile)

  nml_list <- RJSONIO::fromJSON(nml_json)
  nml_dir <- file.path(tempdir(), 'nml')
  dir.create(nml_dir)
  site_ids <- names(nml_list)

  nml_files <- sapply(site_ids, function(x){
    class(nml_list[[x]]) <- 'nml'
    nml_file <- paste0(x, "_glm3.nml")
    glmtools::write_nml(nml_list[[x]], file = file.path(nml_dir, nml_file))
    return(nml_file)
  })

  setwd(nml_dir)

  if (file.exists(zippath)){
    unlink(zippath)
  }

  zip(zippath, files = nml_files)

  unlink(nml_dir, recursive = TRUE)
  setwd(cd)
}

group_meteo_fls <- function(meteo_dir, groups, counties_sf, use_states){

  # turn files into point locations
  # check group match with assign_group_id(points, polygons)
  # return data.frame with id and filename

  meteo_fls <- data.frame(files = dir(meteo_dir), stringsAsFactors = FALSE) %>%
    filter(stringr::str_detect(files, "[0-9n]\\].csv")) %>%
    mutate(x = stringr::str_extract(files, 'x\\[[0-9]+\\]') %>% str_remove('x\\[') %>% str_remove('\\]') %>% as.numeric(),
           y = stringr::str_extract(files, 'y\\[[0-9]+\\]') %>% str_remove('y\\[') %>% str_remove('\\]') %>% as.numeric()) %>%
    left_join(suppressWarnings(st_centroid(create_ldas_grid()))) %>% rename(geometry = ldas_grid_sfc) %>% select(-x, -y) %>%
    st_sf()

  state_meteo_rows <- counties_sf %>% group_by(state) %>% summarise() %>% filter(state %in% use_states) %>%
    st_buffer(0.07) %>% # degree buffer to extend the state to include those meteo cells too
    suppressWarnings() %>% st_covers(y = meteo_fls) %>% suppressWarnings() %>% as.data.frame() %>% pull(col.id)

  grouped_df <- st_intersects(x = meteo_fls, y = groups) %>% as.data.frame() %>% rename(group_idx = col.id) %>% suppressWarnings()

  meteo_fls %>% mutate(row.id = row_number()) %>%
    filter(row.id %in% state_meteo_rows) %>%
    inner_join(grouped_df) %>% mutate(group_id = groups$group_id[group_idx], meteo_filepath = file.path(meteo_dir, files)) %>%
    select(meteo_filepath, group_id) %>% st_drop_geometry() %>% suppressWarnings()

}

zip_meteo_groups <- function(outfile, xwalk_meteo_fl_names, grouped_meteo_fls){

  cd <- getwd()
  on.exit(setwd(cd))

  groups <- unique(grouped_meteo_fls$group_id)

  data_files <- c()
  for (group in groups){
    meteo_dir <- file.path(tempdir(), group)
    dir.create(meteo_dir)

    zipfile <- paste0('tmp/inputs_', group, '.zip')
    these_files <- grouped_meteo_fls %>% filter(group_id == !!group) %>%
      mutate(pipeline_fl = basename(meteo_filepath)) %>%
      inner_join(xwalk_meteo_fl_names, by = 'pipeline_fl')
    # write these files under the release name in a tempdir:
    for (i in 1:length(these_files$meteo_filepath)){
      data.table::fread(these_files$meteo_filepath[i], nrows = 14976) %>%
        data.table::fwrite(file.path(meteo_dir, these_files$release_fl[i]))
    }

    zippath <- file.path(getwd(), zipfile)

    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }

    setwd(meteo_dir)
    zip(zippath, files = these_files$release_fl)
    setwd(cd)
    data_files <- c(data_files, zipfile)
    unlink(meteo_dir, recursive = TRUE)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
}

filter_resample_obs <- function(outfile, remove_sites, obs_feather, site_ids, obs_start, obs_stop, sample_res = 0.5){
  
  feather::read_feather(obs_feather) %>%
    filter(site_id %in% site_ids & !site_id %in% remove_sites) %>% 
    filter(date >= obs_start & date <= obs_stop) %>%
    group_by(date, depth, site_id, source) %>%
    summarize(temp = mean(temp)) %>%
    ungroup() %>%
    # interpolate
    group_by(site_id, date, source) %>%
    do({
      date_df <- .
      date_df %>%
        mutate(
          # find the closest PGDL depth to the observation depth
          new_depth = purrr::map_dbl(depth, function(obsdep) {
            # get depths from 0 to max depth of obs, rounded up to nearest sample_res, but not beyond
            # so if obsdep = c(0.3, 9.6), we'll go to 10m, but if obsdep = c(0.3, 9.5), we'd go to 9.5
            depths <- seq(0, max(ceiling(obsdep / sample_res) * sample_res), by = sample_res)
            depths[which.min(abs(obsdep - depths))]
            }),
          # estimate temperature at the new depth using interpolation, or if
          # that's not possible, set to the nearest observed temperature
          new_temp = if(nrow(date_df) >= 2) {
            # if we have enough values on this date, interpolate
            approx(x=depth, y=temp, xout=new_depth, rule=2)$y
          } else {
            # if we can't interpolate, just use the nearest value
            temp
          },
          depth_diff = abs(depth - new_depth)) %>%
        # after approx(), trash any values at new_depth >= 0.5 m from the nearest observation
        filter(depth_diff < sample_res) %>%
        # only keep one estimate for each new_depth
        group_by(new_depth) %>%
        filter(depth_diff == min(depth_diff)) %>%
        ungroup()
    }) %>%
    ungroup() %>%
    # to see my work as columns, print out the result up to this point, e.g. by uncommenting
    # tail(20) %>% print(n=20)
    # now we clean up the columns
    select(site_id, date, depth=new_depth, temp=new_temp, source) %>%
    # take care of some duplicated depth-date combos, such as when orig depths were 0.4 and 0.5 and sample_res is 0.5
    group_by(site_id, date, depth) %>%
    summarize(temp = first(temp), source = first(source)) %>%
    ungroup() %>% 
    saveRDS(file = outfile)
}

filter_csv_obs <- function(outfile, obs_csv, site_ids, obs_start, obs_stop){
  readr::read_csv(obs_csv) %>%
    filter(site_id %in% site_ids) %>%
    filter(date >= obs_start & date <= obs_stop) %>%
    saveRDS(file = outfile)
}

#' builds the data.frame that is used to define how model results are exported
#' @param site_ids which model ids to use in the export
#' @param file_template the pattern for how to write the source_filepath given the site_id
#' @param exp_prefix prefix to the exported files (e.g., 'pb0')
#' @param exp_suffix suffix to the exported files (e.g., 'irradiance')
export_pb_df <- function(site_ids, file_template, exp_prefix, exp_suffix, dummy){

  tibble(site_id = site_ids) %>% 
    mutate(source_filepath = sprintf(file_template, site_id), hash = tools::md5sum(source_filepath)) %>% 
    mutate(out_file = sprintf('%s_%s_%s.csv', exp_prefix, site_id, exp_suffix)) %>% 
    select(site_id, source_filepath, out_file, hash)
}

export_from_table <- function(model_out_fl, exp_prefix, exp_suffix){
  source_dir <- paste0('../', str_split(dirname(model_out_fl), '/')[[1]][2])
  stopifnot(dir.exists(source_dir)) # safety check since this is a hacky way to get the dir
  model_info <- yaml::yaml.load_file(model_out_fl)

  tibble(file = names(model_info), hash = unlist(model_info)) %>% 
    split_pb_filenames() %>% 
    mutate(source_filepath = file.path(source_dir, file), out_file = sprintf('%s_%s_%s.csv', exp_prefix, site_id, exp_suffix)) %>% 
    select(site_id, source_filepath, out_file, hash)
}

export_pbmtl_df <- function(model_out_fl, exp_prefix, exp_suffix){
  source_dir <- paste0('../', str_split(dirname(model_out_fl), '/')[[1]][2])
  stopifnot(dir.exists(source_dir)) # safety check since this is a hacky way to get the dir
  model_info <- yaml::yaml.load_file(model_out_fl)
  
  # need to extract "nhdhr_143418975" from "4_transfer/out/nhdhr_143418975_t|s_nhdhr_143418891_temperatures.feather"
  tibble(source_filename = names(model_info), hash = unlist(model_info)) %>% rowwise() %>% 
    mutate(site_id = {str_split(basename(source_filename), '_t\\|s_')[[1]][1]}) %>% ungroup() %>% 
    mutate(source_filepath = file.path(source_dir, source_filename), out_file = sprintf('%s_%s_%s.csv', exp_prefix, site_id, exp_suffix)) %>% 
    select(site_id, source_filepath, out_file, hash)
}

export_mtl_df <- function(site_ids, dir_template, file_pattern, exp_prefix, exp_suffix, source_file, dummy){
  
  get_recent_top_source <- function(dir, file_pattern){
    files <- tibble(file = dir(dir)) %>% filter(stringr::str_detect(file, file_pattern)) %>% pull(file)
    if (length(files) == 1)
      return(file.path(dir, files[1L]))
    else {
      # this is hacky, but take the NEWEST source pair
      tibble(file = files) %>% mutate(mtime = {file.info(file.path(dir, file))$mtime}) %>% arrange(desc(mtime)) %>% 
        head(1L) %>% pull(file) %>% file.path(dir, .)
    }
  }
  
  tibble(site_id = site_ids) %>% 
    mutate(source_dir = sprintf(dir_template, site_id)) %>% rowwise() %>% 
    #file.path(source_dir, {tibble(file = dir(source_dir)) %>% filter(stringr::str_detect(file, file_pattern)) %>% pull(file)})
    mutate(source_filepath = get_recent_top_source(source_dir, file_pattern)) %>% 
    ungroup() %>% mutate(hash = tools::md5sum(source_filepath)) %>% 
    mutate(out_file = sprintf('%s_%s_%s.csv', exp_prefix, site_id, exp_suffix)) %>% 
    select(site_id, source_filepath, out_file, hash)
}
export_mtl9_df <- function(site_ids, dir_template, file_pattern, exp_prefix, exp_suffix, dummy){
  
  
  tibble(site_id = site_ids) %>% 
    mutate(source_dir = sprintf(dir_template, site_id)) %>% rowwise() %>% 
    mutate(source_filepath = file.path(source_dir, {tibble(file = dir(source_dir)) %>% filter(stringr::str_detect(file, file_pattern)) %>% pull(file)})) %>% 
    ungroup() %>% mutate(hash = tools::md5sum(source_filepath)) %>% 
    mutate(out_file = sprintf('%s_%s_%s.csv', exp_prefix, site_id, exp_suffix)) %>% 
    select(site_id, source_filepath, out_file, hash)
}

zip_mtl_export_groups <- function(outfile, file_info_df, site_groups,
                                 export = c('pgmtl_predictions','pgmtl9_predictions')){
  
  export <- match.arg(export)
  
  model_feathers <- inner_join(file_info_df, site_groups, by = 'site_id') %>%
    select(-site_id)
  
  zip_pattern <- paste0('tmp/', export, '_%s.zip')
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  groups <- rev(sort(unique(model_feathers$group_id)))
  data_files <- c()
  
  for (group in groups){
    zipfile <- sprintf(zip_pattern, group)
    
    these_files <- model_feathers %>% filter(group_id == !!group)
    
    zippath <- file.path(getwd(), zipfile)
    
    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }
    
    for (i in 1:nrow(these_files)){
      fileout <- file.path(tempdir(), these_files$out_file[i])
      
      feather::read_feather(these_files$source_filepath[i]) %>% 
        # files are of the form
        # A tibble: 6 x 21
        # index      `0.0` `0.5` `1.0` `1.5` `2.0` `2.5` `3.0` ...
        rename_at(vars(-1), function(x)paste0('temp_',x)) %>% # rename column names
        mutate(date = as.Date(index)) %>% 
        write_csv(path = fileout)
    }
    
    setwd(tempdir())
    
    zip(zippath, files = these_files$out_file)
    unlink(these_files$out_file)
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)
  
}


zip_pb_export_groups <- function(outfile, file_info_df, site_groups,
                                 export = c('ice_flags','pb0_predictions','pball_predictions', 'pbmtl_predictions'),
                                 export_start, export_stop){

  export <- match.arg(export)

  model_feathers <- inner_join(file_info_df, site_groups, by = 'site_id') %>%
    select(-site_id)

  zip_pattern <- paste0('tmp/', export, '_%s.zip')

  cd <- getwd()
  on.exit(setwd(cd))

  groups <- rev(sort(unique(model_feathers$group_id)))
  data_files <- c()

  for (group in groups){
    zipfile <- sprintf(zip_pattern, group)

    these_files <- model_feathers %>% filter(group_id == !!group)

    zippath <- file.path(getwd(), zipfile)

    if (file.exists(zippath)){
      unlink(zippath) #seems it was adding to the zip as opposed to wiping and starting fresh...
    }

    for (i in 1:nrow(these_files)){
      fileout <- file.path(tempdir(), these_files$out_file[i])
      
      model_data <- feather::read_feather(these_files$source_filepath[i]) %>%
        mutate(date = as.Date(lubridate::floor_date(DateTime, 'days'))) %>%
        filter(date >= export_start & date <= export_stop)

      switch(export,
             ice_flags = select(model_data, date, ice),
             pb0_predictions = select(model_data, date, contains('temp_')),
             pbmtl_predictions = select(model_data, date, contains('temp_')), # NEED TO EXTEND???
             pball_predictions = select(model_data, date, contains('temp_'))) %>% 
        write_csv(path = fileout)
    }

    setwd(tempdir())

    zip(zippath, files = these_files$out_file)
    unlink(these_files$out_file)
    setwd(cd)
    data_files <- c(data_files, zipfile)
  }
  scipiper::sc_indicate(outfile, data_file = data_files)

}

reshape_sparse_PGDL_csv <- function(outfile, infile){
  # get rid of the unnamed index/row column
  read_csv(infile) %>% select(-X1) %>% 
    mutate(site_id = paste0('nhdhr_', site_id)) %>% 
    pivot_longer(cols = ends_with(' obs median'), names_to = 'n_prof_name', values_to = "median_rmse") %>% 
    mutate(n_prof = as.numeric(str_remove(n_prof_name, ' obs median'))) %>% 
    select(site_id, n_prof, median_rmse) %>% 
    # remove models that were not evaluated because they didn't exist. Jared set these as RMSE = 0:
    filter(median_rmse > 0) %>% 
    write_csv(path = outfile)
}

reshape_join_mtl_metafeats <- function(outfile, pbmtl_file, pgdlmtl_file, joint_features){
  pbmtl_feats <- read_csv(pbmtl_file) %>% select(-`pb-mtl_rmse`, -predicted_rmse) %>% 
    rename(diff_max_depth = dif_max_depth, diff_surface_area = dif_surface_area, 
           diff_obs_temp_air = obs_temp_mean_airdif,
           diff_sw_mean_au = dif_sw_mean_au, diff_ws_mean_au = dif_ws_mean_au, 
           diff_lathrop_strat = dif_lathrop_strat, diff_glm_strat_perc = dif_glm_strat_perc,
           abs_diff_glm_strat_perc = ad_glm_strat_perc, perc_diff_max_depth = perc_dif_max_depth,
           perc_diff_surface_area = perc_dif_surface_area)
  
  
  read_csv(pgdlmtl_file) %>% select(-`pg-mtl_rmse`, -predicted_rmse) %>% 
    rename(n_obs = source_observations, obs_temp_mean = mean_source_observation_temp, 
           diff_rh_mean_au = diff_RH_mean_autumn, diff_glm_strat_perc = dif_glm_strat_perc, 
           perc_diff_max_depth = percent_diff_max_depth, perc_diff_surface_area = percent_diff_surface_area,
           perc_diff_sqrt_surface_area = perc_dif_sqrt_surface_area) %>% 
    # removed the redundant features:
    select(-one_of(joint_features)) %>% 
    left_join(pbmtl_feats, by = c('target_id','source_id')) %>%
    select(target_id, source_id, diff_max_depth, perc_diff_max_depth, diff_glm_strat_perc, 
           diff_surface_area, perc_diff_surface_area, obs_temp_mean, n_obs, perc_diff_sqrt_surface_area, 
           diff_lathrop_strat, diff_rh_mean_au, diff_obs_temp_air, diff_ws_mean_au, 
           abs_diff_glm_strat_perc, obs_temp_kurt, diff_sw_mean_au, obs_temp_skew) %>% 
    write_csv(path = outfile)
  
}

zip_this <- function(outfile, .object){

  if ('data.frame' %in% class(.object)){
    filepath <- basename(outfile) %>% tools::file_path_sans_ext() %>% paste0('.csv') %>% file.path(tempdir(), .)
    write_csv(.object, path = filepath)
    zip_this(outfile = outfile, .object = filepath)
  } else if (class(.object) == 'character' & file.exists(.object)){
    # for multiple files?
    curdir <- getwd()
    on.exit(setwd(curdir))
    setwd(dirname(.object))
    zip(file.path(curdir, outfile), files = basename(.object))
  } else {
    stop("don't know how to zip ", .object)
  }
}

zip_filter_obs <- function(outfile, in_file){

  zip_this(outfile, .object = readRDS(in_file))

}
