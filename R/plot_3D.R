#' Create a 3D Principal Component Scores Plot
#'
#' This function creates a 3D PC scores plot using the designated principal
#' componenets. There is a option to use a preset colours palette or to
#' provide your own. The 3D pc scores plot will be exported as a html widget.
#'
#' @param df The dataframe holding the PC scores data
#' @param variable The variable used to describe the data (legend in PC scores
#' plot)
#' @param pc_num_1 The first principal component to be used
#' @param pc_num_2 The second principal component to be used
#' @param pc_num_3 The third principal component to be used
#' @param colours A provided list of colours for a colour palette if the
#' preset colour palette is not desired. This list must be separated using ', '
#' (e.g 'red, green, blue'). The number of colours provided must be equal to or
#' greater than the number of unique classes for the variable chosen.
#' @return A HTML widget for the 3D PC scores plot
#' @export
plot_3D <- function(df, variable, pc_num_1, pc_num_2, pc_num_3, colours = NULL){
  if(is.null(colours)){
    fig_3D <- plotly::plot_ly(df, x = df[[paste('PC', pc_num_1, sep = '')]], y = df[[paste('PC', pc_num_2, sep = '')]], z = df[[paste('PC', pc_num_3, sep = '')]], color = df[[variable]],
                              colors = rainbow(n_distinct(df[[variable]]))) %>%
      plotly::add_markers(size = 12)

    fig_3D <- fig_3D %>%
      plotly::layout(
        title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
        scene = list(bgcolor = "#FFFFFF",
                     aspectmode = "cube",
                     xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_1, sep = '')),
                     yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_2, sep = '')),
                     zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste('PC', pc_num_3, sep = '')))
      )

    fig_3D

    htmlwidgets::saveWidget(fig_3D, file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
  } else{
    col_list <- as.data.frame(strsplit(colours, ', '))
    colnames(col_list)[1] <- 'Colours'

    fig_3D <- plotly::plot_ly(df, x = df[[paste('PC', pc_num_1, sep = '')]], y = df[[paste('PC', pc_num_2, sep = '')]], z = df[[paste('PC', pc_num_3, sep = '')]], color = df[[variable]],
                              colors = col_list[['Colours']]) %>%
      plotly::add_markers(size = 12)

    fig_3D <- fig_3D %>%
      plotly::layout(
        title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
        scene = list(bgcolor = "#FFFFFF",
                     aspectmode = "cube",
                     xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_1, sep = '')),
                     yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_2, sep = '')),
                     zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste('PC', pc_num_3, sep = '')))
      )

    fig_3D

    htmlwidgets::saveWidget(fig_3D, file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
  }
}
