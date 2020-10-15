library(tidyverse)
library(ggplot2)

lake_metadata <- read_csv('~/Downloads/lake_metadata (5).csv')
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
png('~/Downloads/fancy_jared_plot.png', width = 9, height = 5.5, units = 'in', res = 230)
par(omi = c(0,0,0,0), mai = c(0,0,0,0))
plot(0, NA, xpd = TRUE, axes = FALSE, ylab = "", xlab = "", xlim = c(1,n_dot_bins*2.3), ylim = c(8,55)) 

offset_mult <- 12
x_cnt <- 0
cex_dot <- 1.4
cex_med <- 0.9
titles <- c('pg_group' = 'PGDL-MTL','pb_group' = 'PB-MTL')
for (model_group in c('pg_group','pb_group')){
  y_cnt <- 1
  for (plot_var_name in rev(c('max_depth','log_surface_area', 'log_n_obs'))){
    message(plot_var_name)
    
    df <- lake_metadata %>% 
      mutate(log_surface_area = log(surface_area), log_n_obs = log(n_obs)) %>% 
      inner_join(top9_counts) %>% filter(.data[[model_group]] %in% c('bottom','top')) %>% 
      mutate(bin = cut(.data[[plot_var_name]], breaks = n_dot_bins, labels = F, right = F)) %>% 
      rename(plot_variable = .data[[plot_var_name]], model_group = .data[[model_group]]) %>% 
      select(bin, plot_variable, model_group, pg_rank) %>% group_by(bin, model_group) %>% 
      arrange(plot_variable) %>% mutate(rank = rank(plot_variable, ties.method = 'first')) %>% 
      mutate(rank = ifelse(model_group == 'bottom', 1-rank, rank), col = ifelse(model_group == 'bottom', "#F8766D", "#00BFC4"))
    
    
    points(df$bin+x_cnt, df$rank+(y_cnt * offset_mult - 0.5), col = df$col, pch = 16, cex = cex_dot)
    
    medians <- df %>% group_by(model_group) %>% summarize(plot_variable = median(plot_variable)) %>% 
      left_join(df) %>% group_by(model_group) %>% filter(row_number() == 1)
    
    points(medians$bin+x_cnt, medians$rank+(y_cnt * offset_mult - 0.5), col = 'black', pch = 18, cex = cex_med)
    
    text(n_dot_bins/2+x_cnt, y_cnt * offset_mult-offset_mult/2 + 1.25, plot_var_name)
    y_cnt <- y_cnt + 1
  }
  text(n_dot_bins/2+x_cnt, 55, titles[[model_group]], cex = 1.4)
  x_cnt <- x_cnt + n_dot_bins+5
}
dev.off()
