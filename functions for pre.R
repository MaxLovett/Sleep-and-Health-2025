# functions for pre
library(readxl)
library(lubridate)
library(tidyverse)
library(writexl)
library(openxlsx)
library(modelsummary)
library(ggsci)
library(rlang)
library(tidyplots)


plot_variable_over_time <- function(
    temp, x, 
    y = "mean_value",
    sd = "sd_value",
    size = "obs", 
    group_var = "weekend", 
    x_breaks = seq(3, 17, 0.5), x_limits = c(3, 15), 
    title_lab = "变量均值随日期变化", 
    x_lab = "日期", y_lab = "均值") 
  {
  
  p = ggplot(data = temp, aes(x = !!sym(x), y = !!sym(y))) +
    
    # 添加标准差阴影（误差带）
    geom_ribbon(aes(ymin = !!sym(y) - !!(sd), ymax = !!sym(y) + !!(sd)), 
                alpha = 0.2, color = NA) +
    
    # 画出均值折线
    geom_line(linewidth = 1.5, alpha = 0.8) +
    
    # 画出均值点
    geom_point(aes(size = !!sym(size), color = !!sym(group_var)), alpha = 0.8) +
    
    labs(title = ttitle_lab, x = x_lab, y = y_lab) +
    
    # x轴连续格式
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits
    ) +
    
    theme_minimal() +
    
    mytheme
  
  # 返回ggplot对象
  return(p)
}


library(tidyverse)
library(tidyplots)
gene_expression |> 
  filter(external_gene_name %in% c("Apol6", "Col5a3", "Bsn", "Fam96b", "Mrps14", "Tma7")) |> 
  tidyplot(x = sample_type, y = expression, color = condition) |> 
  add_violin() |> 
  add_data_points_beeswarm(white_border = TRUE) |> 
  adjust_x_axis_title("") |> 
  remove_legend() |> 
  add_test_asterisks(hide_info = TRUE, bracket.nudge.y = 0.3) |> 
  adjust_colors(colors_discrete_ibm) |> 
  adjust_y_axis_title("Gene expression") |> 
  split_plot(by = external_gene_name, ncol = 2)
