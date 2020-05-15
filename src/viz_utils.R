plot_rmses <- function(out_file, pb0_rmse_file, pgdl_rmse_file) {
  pb0_rmse <- read_csv(pb0_rmse_file, col_types='cd')
  pgdl_rmse <- read_csv(pgdl_rmse_file, col_types='cd')
  rmses <- left_join(pgdl_rmse, pb0_rmse, by='site_id', suffix=c('_pgdl','_pb0'))
  ggplot(rmses, aes(x=rmse_pb0, y=rmse_pgdl)) + geom_abline() + geom_point(color='royalblue') + coord_fixed() + theme_bw()
  ggsave(out_file, width=6, height=6)
}
