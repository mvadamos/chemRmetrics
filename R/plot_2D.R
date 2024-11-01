#' Create a 2D Principal Component Scores Plot
#'
#' This function creates a 2D PC scores plot using the designated principal
#' componenets. There is a option to use a preset colours palette or to
#' provide your own. The 2D pc scores plot will be exported as a html widget.
#'
#' @param df The dataframe holding the PC scores data
#' @param variable The variable used to describe the data (legend in PC scores
#' plot)
#' @param pc_num_1 The first principal component to be used
#' @param pc_num_2 The second principal component to be used
#' @param colours A provided list of colours for a colour palette if the
#' preset colour palette is not desired. This list must be separated using ', '
#' (e.g 'red, green, blue'). The number of colours provided must be equal to or
#' greater than the number of unique classes for the variable chosen.
#' @return A HTML widget for the 2D PC scores plot
#' @export
plot_2D <- function(df, variable, pc_num_1, pc_num_2, colours = NULL){

  if(is.null(colours)){
    fig_2D <- plotly::plot_ly(df, x = df[[paste('PC', pc_num_1, sep = '')]], y = df[[paste('PC', pc_num_2, sep = '')]], type = 'scatter', mode = 'markers', color = df[[variable]],
                              colors = rainbow(n_distinct(df[[variable]])))

    fig_2D <- fig_2D %>%
      plotly::layout(
        title = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ' '), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), varaible, sep = ''), ')', sep = ''),
        scene = list(bgcolor = "#FFFFFF"),
        xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = paste('PC', pc_num_1, sep = '')),
        yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside',title = paste('PC', pc_num_2, sep = '')))

    fig_2D

    htmlwidgets::saveWidget(fig_2D, file = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
  } else{
    col_list <- as.data.frame(strsplit(colours, ', '))
    colnames(col_list)[1] <- 'Colours'


    fig_2D <- plotly::plot_ly(df, x = df[[paste('PC', pc_num_1, sep = '')]], y = df[[paste('PC', pc_num_2, sep = '')]], type = 'scatter', mode = 'markers', color = df[[variable]],
                              colors = col_list[['Colours']])

    fig_2D <- fig_2D %>%
      plotly::layout(
        title = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ' '), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), varaible, sep = ''), ')', sep = ''),
        scene = list(bgcolor = "#FFFFFF"),
        xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = paste('PC', pc_num_1, sep = '')),
        yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside', title = paste('PC', pc_num_2, sep = '')))

    fig_2D

    htmlwidgets::saveWidget(fig_2D, file = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
  }
}
