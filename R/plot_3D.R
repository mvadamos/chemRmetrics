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
#' @param msize The variable used to adjust the size of the markers, must be a numeric
#' value (default = 12)
#' @param colours A provided colour palette if the preset colour palette is not desired.
#' This palette must be prepared as follows: colours = c('colour1', 'colour2', ...)
#' The number of colours provided must be equal to or greater than the number of unique classes
#' for the variable chosen.
#' @param proj Set to TRUE if the data being plotted involves a projected
#' set of data (default = FALSE). This will change the marker symbol of all projected data to
#' a diamond.
#' @return A HTML widget for the 3D PC scores plot
#' @export
plot_3D <- function(df, variable, pc_num_1, pc_num_2, pc_num_3, msize = 12, colours = NULL, proj = FALSE){
  # Checks for projected data
  if (proj == TRUE){
    # Checks for custom colour palette
    if (is.null(colours)){
      # Creating 3D scatter plot using PC scores data
      fig_3D <- plotly::plot_ly(df,
                                # Providing a rainbow colour palette as default
                                colors = rainbow(dplyr::n_distinct(df[[variable]])),
                                # Selecting symbols for original and projected data
                                symbols = c('Original' = 'circle', 'Projected' = 'diamond'),
                                # Setting plot type as a scatter plot
                                type = 'scatter3d',
                                # Selecting plot mode as markers
                                mode = 'markers') |>
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            z = df[[paste('PC', pc_num_3, sep = '')]],
                            # Colours are determined using the variable column specified using the 'variable' argument
                            color = df[[variable]],
                            # Symbols are determined using the 'proj' column creating with the 'pca()' or pca_proj() functions
                            symbol = ~proj,
                            # Size of the marker specified using the 'msize' argument
                            marker = list(size = msize)
        )

      fig_3D <- fig_3D |>
        # Establishing the plot layout
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF",
                       aspectmode = "cube",
                       xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))
        )

      fig_3D

      # Exporting 3D scatter plot as a html file
      htmlwidgets::saveWidget(fig_3D, file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    } else{
      fig_3D <- plotly::plot_ly(df,
                                # Colour palette set as custom colour palette specified using the 'colours' argument
                                colors = colours,
                                symbols = c('Original' = 'circle', 'Projected' = 'diamond'),
                                type = 'scatter3d',
                                mode = 'markers') |>
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            z = df[[paste('PC', pc_num_3, sep = '')]],
                            color = df[[variable]],
                            symbol = ~proj,
                            marker = list(size = msize)
        )

      fig_3D <- fig_3D |>
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF",
                       aspectmode = "cube",
                       xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))
        )

      fig_3D

      htmlwidgets::saveWidget(fig_3D, file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    }
  } else{
    # Same code as for (proj = TRUE), with no 'symbol' or 'symbols' arguments
    if (is.null(colours)){
      fig_3D <- plotly::plot_ly(df,
                                colors = rainbow(dplyr::n_distinct(df[[variable]])),
                                type = 'scatter3d',
                                mode = 'markers') |>
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            z = df[[paste('PC', pc_num_3, sep = '')]],
                            color = df[[variable]],
                            marker = list(size = msize)
        )

      fig_3D <- fig_3D |>
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF",
                       aspectmode = "cube",
                       xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))
        )

      fig_3D

      htmlwidgets::saveWidget(fig_3D, file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    } else{
      fig_3D <- plotly::plot_ly(df,
                                colors = colours,
                                type = 'scatter3d',
                                mode = 'markers') |>
        plotly::add_markers(x = df[[paste('PC', pc_num_1, sep = '')]],
                            y = df[[paste('PC', pc_num_2, sep = '')]],
                            z = df[[paste('PC', pc_num_3, sep = '')]],
                            color = df[[variable]],
                            marker = list(size = msize)
        )

      fig_3D <- fig_3D |>
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF",
                       aspectmode = "cube",
                       xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                       zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')))
        )

      fig_3D

      htmlwidgets::saveWidget(fig_3D, file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').html', sep = ''))
    }
  }
}
