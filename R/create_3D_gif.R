#' Create a GIF of a Rotating 3D Principal Component Scores Plot
#'
#' This function creates a rotating gif of a 3D pc scores plot. The 'plotly'
#' package is used to create a html widget of the 3D pc score plot. Trigonometric
#' functions are used to rotate the camera of the html widget. The 'webshot2'
#' package is then used to screenshot the html widgets at each point of rotation.
#' These png screenshots are then turned into a gif, using the 'gifski' package.
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
#' @param num_images The number of images to be collated to form the gif file
#' @return A gif of a rotating 3D pc scores plot
#' @export
create_3D_gif <- function(df, variable, pc_num_1, pc_num_2, pc_num_3, msize = 12, colours = NULL, proj = FALSE, num_images){
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

      # Rotating the plot for each plot up to the number of images specified to create the gif using the 'num_images' argument
      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        plot_iteration <- paste('image', i/((2*pi)/num_images), sep = '_')
        fig_3D <- fig_3D |>
          # Establishing the plot layout
          plotly::layout(
            title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
            scene = list(bgcolor = "#FFFFFF",
                         aspectmode = "cube",
                         xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         # Rotating the plot
                         camera = list(eye = list(x = cos(i)*2, y = sin(i)*2, z = 0.2)))
          )

        fig_3D

        # Exporting 3D scatter plot as a html file
        htmlwidgets::saveWidget(plotly::as_widget(fig_3D), paste(plot_iteration, '.html', sep = ''))
        # Opening the plot in the viewport and taking a screenshot (webshot), saved as a png file
        webshot2::webshot(paste(plot_iteration, '.html', sep = ''), file = paste(plot_iteration, '.png', sep = ''), cliprect = 'viewport')
        # Removing html file of 3D scatter plot
        file.remove(paste(plot_iteration, '.html', sep = ''))
      }

      # Sorting webshot images in order of plot iteration
      png_files <- list.files('.', pattern = '*.png$', full.names = TRUE)
      png_files_sorted <- gtools::mixedsort(png_files)
      # Creating a gif using the webshot images stored in the png files
      gifski::gifski(png_files_sorted, gif_file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').gif', sep = ''), width = 1200, height = 1050, delay = 0.01)
      # Removing the png files after the gif has been created
      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        file.remove(paste(paste('image', i/((2*pi)/num_images), sep = '_'), '.png', sep = ''))
      }
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

      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        plot_iteration <- paste('image', i/((2*pi)/num_images), sep = '_')
        fig_3D <- fig_3D |>
          plotly::layout(
            title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
            scene = list(bgcolor = "#FFFFFF",
                         aspectmode = "cube",
                         xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         camera = list(eye = list(x = cos(i)*2, y = sin(i)*2, z = 0.2)))
          )

        fig_3D

        htmlwidgets::saveWidget(plotly::as_widget(fig_3D), paste(plot_iteration, '.html', sep = ''))
        webshot2::webshot(paste(plot_iteration, '.html', sep = ''), file = paste(plot_iteration, '.png', sep = ''), cliprect = 'viewport')
        file.remove(paste(plot_iteration, '.html', sep = ''))
      }

      png_files <- list.files('.', pattern = '*.png$', full.names = TRUE)
      png_files_sorted <- gtools::mixedsort(png_files)
      gifski::gifski(png_files_sorted, gif_file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').gif', sep = ''), width = 1200, height = 1050, delay = 0.01)
      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        file.remove(paste(paste('image', i/((2*pi)/num_images), sep = '_'), '.png', sep = ''))
      }
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

      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        plot_iteration <- paste('image', i/((2*pi)/num_images), sep = '_')
        fig_3D <- fig_3D |>
          plotly::layout(
            title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
            scene = list(bgcolor = "#FFFFFF",
                         aspectmode = "cube",
                         xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         camera = list(eye = list(x = cos(i)*2, y = sin(i)*2, z = 0.2)))
          )

        fig_3D

        htmlwidgets::saveWidget(plotly::as_widget(fig_3D), paste(plot_iteration, '.html', sep = ''))
        webshot2::webshot(paste(plot_iteration, '.html', sep = ''), file = paste(plot_iteration, '.png', sep = ''), cliprect = 'viewport')
        file.remove(paste(plot_iteration, '.html', sep = ''))
      }

      png_files <- list.files('.', pattern = '*.png$', full.names = TRUE)
      png_files_sorted <- gtools::mixedsort(png_files)
      gifski::gifski(png_files_sorted, gif_file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').gif', sep = ''), width = 1200, height = 1050, delay = 0.01)
      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        file.remove(paste(paste('image', i/((2*pi)/num_images), sep = '_'), '.png', sep = ''))
      }
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

      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        plot_iteration <- paste('image', i/((2*pi)/num_images), sep = '_')
        fig_3D <- fig_3D |>
          plotly::layout(
            title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
            scene = list(bgcolor = "#FFFFFF",
                         aspectmode = "cube",
                         xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_1, sep = ''), paste(paste(paste( '(', df[pc_num_1 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_2, sep = ''), paste(paste(paste( '(', df[pc_num_2 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste(paste('PC', pc_num_3, sep = ''), paste(paste(paste( '(', df[pc_num_3 + 1, ncol(df)], sep = ''), sep = ''), ')', sep = ''), sep = ' ')),
                         camera = list(eye = list(x = cos(i)*2, y = sin(i)*2, z = 0.2)))
          )

        fig_3D

        htmlwidgets::saveWidget(plotly::as_widget(fig_3D), paste(plot_iteration, '.html', sep = ''))
        webshot2::webshot(paste(plot_iteration, '.html', sep = ''), file = paste(plot_iteration, '.png', sep = ''), cliprect = 'viewport')
        file.remove(paste(plot_iteration, '.html', sep = ''))
      }

      png_files <- list.files('.', pattern = '*.png$', full.names = TRUE)
      png_files_sorted <- gtools::mixedsort(png_files)
      gifski::gifski(png_files_sorted, gif_file = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ').gif', sep = ''), width = 1200, height = 1050, delay = 0.01)
      for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
        file.remove(paste(paste('image', i/((2*pi)/num_images), sep = '_'), '.png', sep = ''))
      }
    }
  }
}
