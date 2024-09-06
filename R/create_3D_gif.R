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
#' @param colours A provided list of colours for a colour palette if the
#' preset colour palette is not desired. This list must be separated using ', '
#' (e.g 'red, green, blue'). The number of colours provided must be equal to or
#' greater than the number of unique classes for the variable chosen.
#' @param num_images The number of images to be collated to form the gif file
#' @return A gif of a rotating 3D pc scores plot
#' @export

create_3D_gif <- function(df, variable, pc_num_1, pc_num_2, pc_num_3, colours = NULL, num_images){
  if (is.null(colours)){
    fig_3D <- plotly::plot_ly(df, x = df[[paste('PC', pc_num_1, sep = '')]], y = df[[paste('PC', pc_num_2, sep = '')]], z = df[[paste('PC', pc_num_3, sep = '')]], color = df[[variable]],
                              colors = rainbow(n_distinct(df[[variable]]))) %>%
      plotly::add_markers(size = 12)

    for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
      plot_iteration <- paste('image', i/((2*pi)/num_images), sep = '_')
      fig_3D <- fig_3D %>%
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF",
                       aspectmode = "cube",
                       xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_1, sep = '')),
                       yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_2, sep = '')),
                       zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste('PC', pc_num_3, sep = '')),
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
    col_list <- as.data.frame(strsplit(colours, ', '))
    colnames(col_list)[1] <- 'Colours'

    fig_3D <- plotly::plot_ly(df, x = df[[paste('PC', pc_num_1, sep = '')]], y = df[[paste('PC', pc_num_2, sep = '')]], z = df[[paste('PC', pc_num_3, sep = '')]], color = df[[variable]],
                              colors = col_list[['Colours']]) %>%
      plotly::add_markers(size = 12)

    for (i in seq(0, 2*pi, by = (2*pi)/num_images)){
      plot_iteration <- paste('image', i/((2*pi)/num_images), sep = '_')
      fig_3D <- fig_3D %>%
        plotly::layout(
          title = paste(paste(paste(paste(paste(paste(paste(paste('PC', pc_num_1, sep = ''), ', PC', sep = ''), pc_num_2, sep = ' '), 'and PC', sep = ' '), pc_num_3, sep = ''), '3D PC Scores Plot (', sep = ' '), variable, sep = ''), ')', sep = ''),
          scene = list(bgcolor = "#FFFFFF",
                       aspectmode = "cube",
                       xaxis = list(zeroline = F,showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_1, sep = '')),
                       yaxis = list(zeroline = F, showline = T, mirror = T, ticks = 'outside', title = paste('PC', pc_num_2, sep = '')),
                       zaxis = list(zeroline = F,showline = T,mirror = T, ticks = 'outside', title = paste('PC', pc_num_3, sep = '')),
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
