
plot_domain_map <- function(fileout, lakes_sf_fl, source_ids, target_test_ids, target_expansion_ids, us_counties_sf, plot_crs = "+init=epsg:2811"){
  all_lakes <- readRDS(lakes_sf_fl) %>% st_transform(crs = plot_crs)
  
  # simplify lakes to speed up the plot
  all_lakes_simple <- sf::st_simplify(all_lakes, dTolerance = 40) %>% mutate(area = st_area(Shape) %>% as.numeric)
  
  conus_states <- group_by(us_counties_sf, state) %>% summarise() %>% st_geometry() %>% st_transform(crs = plot_crs)
  
  source_col <- '#e41a1c'
  test_t_col <- '#4daf4a'
  exp_t_col <- '#984ea3'
  
  png(filename = fileout, width = 7, height = 4.75, units = 'in', res = 350)
  par(omi = c(0,0,0,0), mai = c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  
  #plot(conus_states[names(conus_states) %in% c('minnesota','wisconsin','michigan')], col = NA, border = 'grey50', lwd = 1.5,
  #     reset = FALSE, expandBB = c(0.01,0.02,0.01,0.05))
  
  # set the viewbox:
  all_modeled_lakes <- filter(all_lakes_simple, site_id %in% c(source_ids, target_test_ids, target_expansion_ids))
  source_lakes <- filter(all_lakes_simple, site_id %in% source_ids)
  target_test_lakes <- filter(all_lakes_simple, site_id %in% target_test_ids)
  target_expansion_lakes <- filter(all_lakes_simple, site_id %in% target_expansion_ids)
  
  plot(st_geometry(all_modeled_lakes), col = NA, border = NA,
       reset = FALSE)
  
   
  plot(st_geometry(all_lakes_simple), col = 'grey70', border = 'grey70', lwd = 0.1, add = TRUE)
  plot(conus_states, col = NA, border = 'grey50', lwd = 1.5, add = TRUE) 
  
  plot_modeled_lakes <- function(lakes_sf, col){
    plot(st_geometry(lakes_sf), col = col, border = col, lwd = 0.2, add = TRUE)
    plot(st_centroid(st_geometry(lakes_sf)), col = paste0(col, 'CC'), lwd = 0.5, add = TRUE, cex = 0.3)
  }
  
  plot_modeled_lakes(target_expansion_lakes, exp_t_col)
  plot_modeled_lakes(target_test_lakes, test_t_col)
  plot_modeled_lakes(source_lakes, source_col)
  
  dev.off()
  
}


plot_grouped_lakes_preview <- function(fileout, spatial_groups, county_bounds, site_ids_grouped, lakes_sf_fl){
  out <- plot_groups(fileout, spatial_groups, county_bounds, lakes_sf_fl)
  all_lakes_simple <- out$sf
  g_styles <- out$style


  modeled_lakes_sf <- inner_join(all_lakes_simple, site_ids_grouped, by = 'site_id') %>%
    left_join(g_styles, by = 'group_id')

  plot(st_geometry(modeled_lakes_sf), col = 'dodgerblue', border = 'dodgerblue', lwd = 0.2, add = TRUE)

  for (j in 1:nrow(spatial_groups)){
    bbox <- st_bbox(spatial_groups[j,])
    n_in_box <- site_ids_grouped %>% filter(group_id == spatial_groups[j,]$group_id) %>%
      nrow()
    text(bbox[1], bbox[2]+0.1, str_extract(spatial_groups[j,]$group_id, '[0-9]{2}'), pos = 4, cex = 0.8, offset = 0.1)
    text(bbox[1], bbox[2]+0.3, paste0('n=',n_in_box), pos = 4, cex = 0.5, offset = 0.1)
  }

  dev.off()

}


plot_grouped_cells_preview <- function(fileout, spatial_groups, county_bounds, site_ids_grouped, lakes_sf_fl, grouped_meteo_fls){


  meteos <- basename(grouped_meteo_fls$meteo_filepath)
  ldas_grid <- create_ldas_grid() %>% mutate(meteo_fl = sprintf('NLDAS_time[0.359420]_x[%s]_y[%s].csv', x, y)) %>%
    filter(meteo_fl %in% meteos)


  plot_groups(fileout, spatial_groups, county_bounds, lakes_sf_fl)

  plot(st_geometry(ldas_grid), col = '#ff00ff1A', border = '#ff00ffB2', lwd = 0.2, add = TRUE)

  for (j in 1:nrow(spatial_groups)){
    bbox <- st_bbox(spatial_groups[j,])
    n_in_box <- grouped_meteo_fls %>% filter(group_id == spatial_groups[j,]$group_id) %>% nrow()

    text(bbox[1], bbox[2]+0.1, str_extract(spatial_groups[j,]$group_id, '[0-9]{2}'), pos = 4, cex = 0.8, offset = 0.1)
    text(bbox[1], bbox[2]+0.3, paste0('n=',n_in_box), pos = 4, cex = 0.5, offset = 0.1)
  }

  dev.off()
}


plot_groups <- function(fileout, spatial_groups, county_bounds, lakes_sf_fl){
  png(filename = fileout, width = 8.4, height = 6, units = 'in', res = 500)
  par(omi = c(0,0,0,0), mai = c(0,0,0,0), xaxs = 'i', yaxs = 'i')

  n <- length(unique(spatial_groups$group_id))
  cols <- c('#a6cee3','#b2df8a','#33a02c','#fb9a99','#fdbf6f','#ff7f00','#cab2d6',
            '#e41a1c','#377eb8','#984ea3','#a65628','#f781bf','#007f7f','#ff00ff')

  col_vector <- rep(cols, ceiling(n/length(cols)))[1:n]
  g_styles <- data.frame(group_id = unique(spatial_groups$group_id), col = col_vector, stringsAsFactors = FALSE)

  spatial_groups <- left_join(spatial_groups, g_styles, by = 'group_id')

  plot(st_geometry(spatial_groups), col = paste0(spatial_groups$col, '4D'), border = 'grey70', lwd = 0.1, reset = FALSE)
  all_lakes_simple <- readRDS(lakes_sf_fl) %>% st_transform(crs = "+init=epsg:2811") %>% sf::st_simplify(dTolerance = 40) %>%
    st_transform(crs = "+init=epsg:4326")

  plot(st_geometry(all_lakes_simple), col = 'grey70', border = 'grey70', lwd = 0.1, add = TRUE)

  plot(st_geometry(spatial_groups), col = paste0(spatial_groups$col, '4D'), border = 'grey70', lwd = 0.1, add = TRUE)

  county_bounds %>% st_geometry() %>%
    plot(col = NA, border = 'grey80', lwd = 0.5, add = TRUE)

  county_bounds %>% group_by(state) %>% summarise() %>% st_geometry() %>%
    plot(col = NA, border = 'grey40', lwd = 2, add = TRUE)

  invisible(list(sf = all_lakes_simple, style = g_styles))
}
