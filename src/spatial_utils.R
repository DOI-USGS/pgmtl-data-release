
fetch_zip_url_sf <- function(zip_url, layer_name){
  
  destination = tempfile(pattern = layer_name, fileext='.zip')
  file <- GET(zip_url, write_disk(destination, overwrite=T), progress())
  shp_path <- tempdir()
  unzip(destination, exdir = shp_path)

  sf::st_read(shp_path, layer=layer_name) %>%
    st_transform(crs = 4326) %>% 
    mutate(state = dataRetrieval::stateCdLookup(STATEFP)) %>% 
    dplyr::select(state, county = NAME)
  
}

sf_names_from_overlap <- function(sf_polys1, sf_polys2){
  
  # will be some NA when the point isn't contained within the polgyon:
  match_idx <- st_transform(sf_polys1, crs = "+init=epsg:2811") %>% sf::st_simplify(dTolerance = 40) %>% 
    st_transform(crs = st_crs(sf_polys2)) %>% 
    st_intersects(sf_polys2) 
  
  stopifnot(sum(sapply(match_idx, is.null)) == 0) # check that all poly1 have matches
  purrr::map(1:nrow(sf_polys1),function(i){
    info <- st_drop_geometry(sf_polys2[match_idx[[i]],])
    cbind(st_drop_geometry(sf_polys1[i,]), 
          lapply(info, function(x) { # lapply across the data.frame columns, squash/combine values when > 1
            paste(sort(unique(x)), collapse = '|')
            }) %>% data.frame(stringsAsFactors = FALSE))
  }) %>% purrr::reduce(bind_rows)
}

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

subset_lake_sf <- function(lakes_sf_fl, site_ids){
  readRDS(lakes_sf_fl) %>% filter(site_id %in% !!site_ids) %>% sf::st_zm()
}


create_ldas_grid <- function(x0 = -124.9375, y0 = 25.0625, x_num = 464, y_num = 224, cell_res = 0.125){
  ldas_crs <- "+init=epsg:4326"
  
  ldas_grid_sfc <- sf::st_make_grid(cellsize = cell_res, n = c(x_num, y_num),
                                    offset = c(x0-cell_res/2, y0-cell_res/2), crs = ldas_crs)
  # cells count left to right, then next row, then left to right
  x_cells <- rep(0:(x_num-1), y_num)
  y_cells <- c(sapply(0:(y_num-1), function(x) rep(x, x_num)))
  
  ldas_grid <- st_sf(data.frame(x = x_cells, y = y_cells), ldas_grid_sfc)
  return(ldas_grid)
}

