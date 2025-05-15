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
#' @param msize The variable used to adjust the size of the markers, must be a numeric
#' value (default = 12)
#' @param colours A provided list of colours for a colour palette if the
#' preset colour palette is not desired. This list must be separated using ', '
#' (e.g 'red, green, blue'). The number of colours provided must be equal to or
#' greater than the number of unique classes for the variable chosen.
#' @param proj Set to TRUE if the data being plotted involves a projected
#' set of data (default = FALSE). This will change the marker symbol of all projected data to
#' a diamond.
#' @return A HTML widget for the 2D PC scores plot
#' @export
plot_2D <- function(df, variable, pc_num_1, pc_num_2, msize = 12, colours = NULL, proj = FALSE){
  # Checks for projected data
  if (proj == TRUE){
    # Checks for custom colour palette
    if(is.null(colours)){
      # Creating 2D scatter plot using PC scores data
      fig_2D <- plotly::plot_ly(df,
                                # Providing a rainbow colour palette as default
                                colors = rainbow(n_distinct(df[[variable]])),
                                # Selecting symbols for original and projected data
                                symbols = c('Original' = 'circle', 'Projected' = 'diamond'),
                                # Setting plot type as a scatter plot
                                type = 'scatter',
                                # Selecting plot mode as markers
                                mode = 'markers') %>%
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            # Colours are determined using the variable column specified using the 'variable' argument
                            color = df[[variable]],
                            # Symbols are determined using the 'proj' column creating with the 'pca()' or pca_proj() functions
                            symbol = ~proj,
                            # Size of the marker specified using the 'msize' argument
                            size = msize)

      fig_2D <- fig_2D %>%
        # Establishing the plot layout
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ' '), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF"),
          xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
          yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))

      fig_2D

      # Exporting 2D scatter plot as a html file
      htmlwidgets::saveWidget(fig_2D, file = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    } else{
      # Creating dataframe for custom colours specified using the 'colours' argument
      col_list <- as.data.frame(strsplit(colours, ', '))
      colnames(col_list)[1] <- 'Colours'

      fig_2D <- plotly::plot_ly(df,
                                # Colour palette set as custom colour palette in col_list specified using the 'colours' argument
                                colors = col_list[['Colours']],
                                symbols = c('Original' = 'circle', 'Projected' = 'diamond'),
                                type = 'scatter',
                                mode = 'markers') %>%
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            color = df[[variable]],
                            symbol = ~proj,
                            size = msize)

      fig_2D <- fig_2D %>%
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ' '), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF"),
          xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
          yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))

      fig_2D

      htmlwidgets::saveWidget(fig_2D, file = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    }
  } else{
    # Same code as for (proj = TRUE), with no 'symbol' or 'symbols' arguments
    if(is.null(colours)){
      fig_2D <- plotly::plot_ly(df,
                                colors = rainbow(n_distinct(df[[variable]])),
                                type = 'scatter',
                                mode = 'markers') %>%
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            color = df[[variable]],
                            size = msize)

      fig_2D <- fig_2D %>%
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ' '), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF"),
          xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
          yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))

      fig_2D

      htmlwidgets::saveWidget(fig_2D, file = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    } else{
      col_list <- as.data.frame(strsplit(colours, ', '))
      colnames(col_list)[1] <- 'Colours'


      fig_2D <- plotly::plot_ly(df,
                                colors = col_list[['Colours']],
                                type = 'scatter',
                                mode = 'markers') %>%
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            color = df[[variable]],
                            size = msize)

      fig_2D <- fig_2D %>%
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ' '), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF"),
          xaxis = list(zeroline = F,showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
          yaxis = list(zeroline = F, showline = T, showgrid = FALSE, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))

      fig_2D

      htmlwidgets::saveWidget(fig_2D, file = paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), 'and PC', sep = ''), pc_num_2, sep = ' '), '2D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    }
  }
}
