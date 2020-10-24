library(tidyverse)
library(ggplot2)

lake_metadata <- read_csv('~/Downloads/lake_metadata (5).csv') %>% 
  mutate(log_surface_area = log(surface_area), log_n_obs = log(n_obs))
mtl_rmses <- read_csv('out_data/all_MTL_RMSE_predictions.csv')


# Rank PGDL-MTL sources according to how many times they were predicted to be used in a top 9
top_rnk_n <- 9 # cut-off for being considered a "top source"
count_cut <- 35 # how many upper and lower lakes to include
n_dot_bins <- 35 # how many horizontal bins are there?

top9_counts <- mtl_rmses %>% group_by(source_id) %>% 
  summarize(pg_top9 = sum(pred_pgdl_mtl_rank <= top_rnk_n), 
            pb_top9 = sum(pred_pb_mtl_rank <= top_rnk_n)) %>% 
  mutate(pg_rank = rank(desc(pg_top9), ties.method = 'first'),
         pb_rank = rank(desc(pb_top9), ties.method = 'first')) %>% 
  mutate(pg_group = 
           case_when(pg_rank %in% 1:count_cut ~ "top",
                     pg_rank %in% seq(from = max(pg_rank), by = -1, to = max(pb_rank) -36) ~ "bottom",
                     TRUE ~ "middle"
                     ),
         pb_group = 
           case_when(pb_rank %in% 1:count_cut ~ "top",
                     pb_rank %in% seq(from = max(pb_rank),  by = -1, to = max(pb_rank) -36) ~ "bottom",
                     TRUE ~ "middle"
           )) %>% ungroup() %>% 
  rename(site_id = source_id)

# -- PG-MTL preds --
# Max Depth Difference: max_depth
# Max Depth Percent Difference
# Surface Area Difference: surface_area
# Surface Area Percent Difference
# Square Root Surface Area Percent Difference
# Number of Source Temperature Observations: n_obs
# Mean Source Observation Temp: obs_temp_mean
# GLM Stratification Difference: glm_strat_perc
# Autumn Relative Humidity Difference: rh_mean_au
# Lathrop Stratification Difference: lathrop_strat (but is binary)

# ylim needs to be the gaps plus the max dot stacks for each one...
png('~/Downloads/fancy_jared_plot.png', width = 9, height = 2.5, units = 'in', res = 230)
par(omi = c(0,0,0,0), mai = c(0,2,0,0))

layout(matrix(1:6, 3,byrow = F))

titles <- c('pg_group' = 'PGDL-MTL','pb_group' = 'PB-MTL')
rank_names <- c('pg_group' = 'pred_pb_mtl_rank', 'pb_group' = 'pred_pgdl_mtl_rank')
for (model_group in c('pg_group', 'pb_group')){
  
  for (plot_var_name in rev(c('max_depth','log_surface_area', 'log_n_obs'))){
    message(plot_var_name)
    
    df <- lake_metadata %>% 
      inner_join(top9_counts, by = 'site_id') %>% filter(.data[[model_group]] %in% c('bottom','top')) %>% 
      group_by(.data[[model_group]]) %>% 
      summarize(med = median(.data[[plot_var_name]]), 
                low = quantile(.data[[plot_var_name]], probs = 0.25),
                high = quantile(.data[[plot_var_name]], probs = 0.75), 
                max = min(c(high+(high-low)*1.5), max(.data[[plot_var_name]])), 
                min = min(c(low-(high-low)*1.5), min(.data[[plot_var_name]])), 
                .groups = 'drop') 
    
    source_ids <- group_by(mtl_rmses, source_id) %>% 
      summarize(top_src = sum(.data[[rank_names[[model_group]]]] == 1)) %>% filter(top_src > 9) %>% 
      pull(source_id)
    
    top_pts <- lake_metadata %>% filter(site_id %in% source_ids) %>% 
      pull(.data[[plot_var_name]])
    bottom <- df %>% filter(.data[[model_group]] == 'bottom')
    top <- df %>% filter(.data[[model_group]] == 'top')
    
    plot(0, NA, xpd = TRUE, axes = FALSE, ylab = "", xlab = "", xlim = c(min(df$min), max(df$max)), ylim = c(-1,1))
    
    
    y_mid_bot <- - 0.3
    lines(c(bottom$max, bottom$min), c(y_mid_bot, y_mid_bot))
    polygon(c(bottom$low, bottom$low, bottom$high, bottom$-high), c(y_mid_bot-0.2, y_mid_bot+0.2, y_mid_bot+0.2, y_mid_bot-0.2), 
            col = 'white', border = NA)
    polygon(c(bottom$low, bottom$low, bottom$high, bottom$high), c(y_mid_bot-0.2, y_mid_bot+0.2, y_mid_bot+0.2, y_mid_bot-0.2), 
            col = 'grey40', density = 20, border = 'black')
    lines(c(bottom$med, bottom$med), c(y_mid_bot+0.3, y_mid_bot-0.3), lwd = 2)
    
    y_mid_top <- 0.3
    lines(c(top$max, top$min), c(y_mid_top, y_mid_top))
    polygon(c(top$low, top$low, top$high, top$high), c(y_mid_top-0.2, y_mid_top+0.2, y_mid_top+0.2, y_mid_top-0.2), 
            col = 'white')
    lines(c(top$med, top$med), c(y_mid_top+0.3, y_mid_top-0.3), lwd = 2)
    points(top_pts, y = rep(y_mid_top, length(top_pts)), col = '#ca0020', pch = 16, cex = 1.1)
    
    text(n_dot_bins/2+x_cnt, y_cnt * offset_mult-offset_mult/2 + 1.25, plot_var_name)
    y_cnt <- y_cnt + 1
  }
  text(n_dot_bins/2+x_cnt, 55, titles[[model_group]], cex = 1.4)
  x_cnt <- x_cnt + n_dot_bins+5
}
dev.off()



mtls <- read_csv('out_data/all_MTL_RMSE_predictions.csv') 
type = 'pb'

common_src <- group_by(mtls, source_id) %>% 
  summarize(top_src = sum(.data[[sprintf("pred_%s_mtl_rank", type)]] == 1)) %>% filter(top_src > 9) %>% 
  pull(source_id)

uncom_src <- group_by(mtls, source_id) %>% 
  summarize(top_src = sum(.data[[sprintf("pred_%s_mtl_rank", type)]] == 1)) %>% filter(top_src %in% 1:3) %>% 
  pull(source_id)

message('common sources: ', type)
filter(mtls, .data[[sprintf("pred_%s_mtl_rank", type)]] == 1, source_id %in% common_src) %>% 
  pull(.data[[sprintf("actual_%s_mtl_rank", type)]]) %>% median

message('uncommon sources:')
filter(mtls, .data[[sprintf("pred_%s_mtl_rank", type)]] == 1, source_id %in% uncom_src) %>% 
  pull(.data[[sprintf("actual_%s_mtl_rank", type)]]) %>% median

