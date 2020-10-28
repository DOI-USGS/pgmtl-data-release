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

get_boxplot_stats <- function(data, boxplot_stat = c('min','max','lower','upper','median')){
  stat_idx <- c('min' = 1, 'max' = 5, 'lower' = 2, 'upper' = 4, 'median' = 3)
  return(boxplot(data, plot = FALSE)$stats[stat_idx[[boxplot_stat]]])
}

# ylim needs to be the gaps plus the max dot stacks for each one...
png('~/Downloads/fancy_jared_plot.png', width = 9, height = 2.5, units = 'in', res = 230)
par(omi = c(0,0,0,0), mai = c(0.3,2,0,0), mgp = c(3,0.2,0), xpd = TRUE)

layout(matrix(1:6, 3,byrow = F))

titles <- c('pg_group' = 'PGDL-MTL','pb_group' = 'PB-MTL')
rank_names <- c('pg_group' = 'pred_pb_mtl_rank', 'pb_group' = 'pred_pgdl_mtl_rank')

plot_details <- list(log_n_obs = 
                       list(xlim = c(log(100), log(20000)),
                            at = c(log(100), log(1000), log(10000), 1000),
                            labels = c('100','1,000','10,000', ''),
                            title = c('Number of','observations (#)')),
                     log_surface_area = 
                       list(xlim = c(log(10000), log(300000000)),
                            at = c(log(10000), log(100000), log(1000000), log(10000000), log(100000000),log(1000000000), 1000),
                            labels = c('0.01','0.1','1', '10', '100', '1000',''),
                            title = c(expression(paste("Surface area (km"^"2",")")))),
                     max_depth =
                       list(xlim = c(0, 43),
                            at = seq(0, 100, by = 10),
                            labels = seq(0, 100, by = 10),
                            title = c('Maximum depth (m)'))
)

x_cnt = 0
y_cnt = 0
for (model_group in c('pg_group', 'pb_group')){
  
  for (plot_var_name in rev(c('max_depth','log_surface_area', 'log_n_obs'))){
    message(plot_var_name)
    
    df <- lake_metadata %>% 
      inner_join(top9_counts, by = 'site_id') %>% filter(.data[[model_group]] %in% c('bottom','top')) %>% 
      group_by(.data[[model_group]]) %>% 
      summarize(med = get_boxplot_stats(.data[[plot_var_name]], boxplot_stat = "median"),
                low = get_boxplot_stats(.data[[plot_var_name]], boxplot_stat = "lower"),
                high = get_boxplot_stats(.data[[plot_var_name]], boxplot_stat = "upper"),
                max = get_boxplot_stats(.data[[plot_var_name]], boxplot_stat = "max"),
                min = get_boxplot_stats(.data[[plot_var_name]], boxplot_stat = "min"),
                .groups = 'drop') 
    
    source_ids <- group_by(mtl_rmses, source_id) %>% 
      summarize(top_src = sum(.data[[rank_names[[model_group]]]] == 1)) %>% filter(top_src > 9) %>% 
      pull(source_id)
    
    top_pts <- lake_metadata %>% filter(site_id %in% source_ids) %>% 
      pull(.data[[plot_var_name]])
    # all possible points/lakes that are in the quartiles:
    bot_high_pts <- lake_metadata %>% inner_join(top9_counts, by = 'site_id') %>% 
      filter(.data[[model_group]] %in% c('bottom','top')) %>% 
      select(.data[[model_group]], .data[[plot_var_name]])
    
    bottom <- df %>% filter(.data[[model_group]] == 'bottom')
    top <- df %>% filter(.data[[model_group]] == 'top')
    
    this_plot <- plot_details[[plot_var_name]]
    
    plot(0, NA, xpd = TRUE, axes = FALSE, ylab = "", xlab = "", xlim = this_plot$xlim, ylim = c(-0.5,0.5))
    axis(side = 1, at = this_plot$at, labels = this_plot$labels, tck = -0.03)
    
    y_mid_bot <- - 0.23
    lines(c(bottom$max, bottom$min), c(y_mid_bot, y_mid_bot))
    polygon(c(bottom$low, bottom$low, bottom$high, bottom$high), c(y_mid_bot-0.2, y_mid_bot+0.2, y_mid_bot+0.2, y_mid_bot-0.2), 
            col = 'white', border = NA)
    polygon(c(bottom$low, bottom$low, bottom$high, bottom$high), c(y_mid_bot-0.2, y_mid_bot+0.2, y_mid_bot+0.2, y_mid_bot-0.2), 
            col = 'grey40', density = 20, border = 'black')
    lines(c(bottom$med, bottom$med), c(y_mid_bot+0.2, y_mid_bot-0.2), lwd = 2, lend = 1)
    bot_outliers <- bot_high_pts %>% 
      filter(.data[[model_group]] == 'bottom', .data[[plot_var_name]] > bottom$max | .data[[plot_var_name]] < bottom$min) %>% 
      pull(.data[[plot_var_name]])
    points(bot_outliers, y = rep(y_mid_bot, length(bot_outliers)), col = 'grey30', pch = 18, cex = 0.8)
    
    y_mid_top <- 0.23
    lines(c(top$max, top$min), c(y_mid_top, y_mid_top))
    polygon(c(top$low, top$low, top$high, top$high), c(y_mid_top-0.2, y_mid_top+0.2, y_mid_top+0.2, y_mid_top-0.2), 
            col = 'white')
    lines(c(top$med, top$med), c(y_mid_top+0.2, y_mid_top-0.2), lwd = 2, lend = 1)
    points(top_pts, y = rep(y_mid_top, length(top_pts)), col = '#ca0020', pch = 16, cex = 1.1)
    top_outliers <- bot_high_pts %>% 
      filter(.data[[model_group]] == 'top', .data[[plot_var_name]] > top$max | .data[[plot_var_name]] < top$min) %>% 
      pull(.data[[plot_var_name]])
    points(top_outliers, y = rep(y_mid_top, length(top_outliers)), col = 'grey30', pch = 18, cex = 0.8)
    
    if (length(this_plot$title) == 1){
      text(this_plot$xlim[1], 0, this_plot$title[1], pos = 2, cex = 1.4, offset = 2)
    } else {
      text(this_plot$xlim[1], 0.18, this_plot$title[1], pos = 2, cex = 1.4, offset = 2)
      text(this_plot$xlim[1], -0.18, this_plot$title[2], pos = 2, cex = 1.4, offset = 2)
    }
    
  }
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

