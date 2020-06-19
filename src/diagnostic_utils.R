
#pb0_matched_to_observations <- scmake('pb0_matched_to_observations')

# create a single plot per lake. Use a different color per source

get_cols <- function(n){
  if (n < 3){
    RColorBrewer::brewer.pal(n, 'Dark2')[1:n]
  } else if (n > 8){
    qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
    unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:n]
  } else{
    RColorBrewer::brewer.pal(n, 'Dark2')
  }
}

plot_pred_obs <- function(df, plot_type = 'side_by_side', bad_diff = 10){
  
  orig_df <- df
  cols <- tibble(source = unique(df$source))
  o_par <- par(no.readonly = TRUE)
  # handle special case where source < 3?
  df <- pivot_longer(df, cols = c("obs","pred"), names_to = 'type')
  pch <- tibble(type = c("obs","pred"), pch = c(16, 23))
  sums <- tibble(source = df$source) %>% group_by(source) %>% tally
  cols <- cols %>% mutate(col = get_cols(length(source)))
  df <- left_join(df, cols, by = 'source') %>% 
    left_join(pch, by = "type") %>% 
    left_join(sums, by = "source") %>% arrange(desc(n))
  z_max <- max(df$depth)
  t_max <- max(df$value)
  t_min <- min(df$value)
  
  if (plot_type == 'compare'){
    plot(filter(df, type == 'obs')$value, filter(df, type == 'pred')$value, col = filter(df, type == 'pred')$col, 
         bg = 'black', pch = 16, axes = F, cex = 0.5, xlim = c(0,30), ylim = c(0,30))
    abline(0,1, lty = 'dashed', col = 'grey40')
    axis(1, at = seq(-10,40, by = 10), tcl = -0.0001, cex = 0.5)
    axis(2, at = seq(-10,40, by = 10), tcl = -0.0001, cex = 0.5)
  } else {
    plot(NA, 0, xlim = c(t_min, t_max), ylim = c(z_max, 0), axes = F)
    bd_segs <- orig_df %>% filter(abs(obs-pred) > bad_diff)
    segments(bd_segs$pred, bd_segs$depth, x1 = bd_segs$obs, bd_segs$depth, lwd = 0.5, col = 'grey70')
    axis(1, at = seq(-10,40, by = 5), tcl = -0.0001, cex = 0.5)
    if (z_max < 5){
      axis(2, at = seq(-10,5, by = 1), tcl = -0.0001, cex = 0.5)
    } else {
      axis(2, at = seq(-10,200, by = 5), tcl = -0.0001, cex = 0.5)
    }
    
    points(df$value, df$depth, pch = df$pch, col = df$col, cex = 4/log(nrow(orig_df)))
    
    
    u <- par("usr")
    
    labels <- sprintf('(n_obs=%s; rmse=%1.3f; n_sources=%s)', 
                      nrow(orig_df), 
                      sqrt(mean((orig_df$obs-orig_df$pred)^2, na.rm=TRUE)),
                      length(unique(orig_df$source)))
    site_id <- unique(df$site_id)
    text(u[1], u[3]-(u[3]-u[4])*0.02, labels = unique(df$site_id), pos = 4, offset = 0.3, cex = ifelse(nchar(site_id) > 30, 0.85, 1))
    text(u[1], u[3]-(u[3]-u[4])*0.08, labels = labels, pos = 4, offset = 0.3, cex = 0.85)
  }
  
}

plot_order <- pb0_matched_to_observations %>% group_by(site_id) %>% summarize(rmse = sqrt(mean((obs-pred)^2, na.rm=TRUE)), n = length(source)) %>% filter(n > 29) %>% arrange(desc(rmse))

pdf("~/Downloads/Rplot%03d.pdf", width = 20, height = 8, onefile = TRUE)
layout(matrix(1:32, 4, byrow = TRUE))
par(omi = c(0.05,0.05,0,0), mai = c(0.1,0.1,0.1,0.1), mgp = c(1,0.05,.1), las = 1)
for (j in 1:1600){
  
  pb0_matched_to_observations %>% filter(site_id == plot_order[j,]$site_id) %>% plot_pred_obs()
  pb0_matched_to_observations %>% filter(site_id == plot_order[j,]$site_id) %>% plot_pred_obs(plot_type = 'compare')
}

dev.off()
