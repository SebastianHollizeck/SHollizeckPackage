#' Make shades
#'
#' Creates different shades of an input colour. Can be either lighter or darker.
#'
#' @param colour seed colour to base the shades on
#' @param n amount of shades to generate
#' @param lighter switch wether to go to darker (\code{FALSE})
#'   or lighter (\code{TRU}E) shades
#'
#' @return \code{n} color values (hex codes)
#' @export
#'
#' @examples
#' #three lighter shades
#' make_shades("goldenrod", n=3)
#'
#' #five darker shades
#' make_shades("goldenrod", n=5, lighter=FALSE)
#'
make_shades <- function(colour, n, lighter = TRUE) {

  if(n < 0){
    stop("n must be at least 0")
  }else if(n == 0){
    warning("n should be bigger than 0")
  }

  # Convert the colour to RGB
  colour_rgb <- grDevices::col2rgb(colour)[, 1]

  # Decide if we are heading towards white or black
  if (lighter) {
    end <- 255
  } else {
    end <- 0
  }

  # Calculate the red, green and blue for the shades
  # we calculate one extra point to avoid pure white/black
  red <- seq(colour_rgb[1], end, length.out = n + 1)[1:n]
  green <- seq(colour_rgb[2], end, length.out = n + 1)[1:n]
  blue <- seq(colour_rgb[3], end, length.out = n + 1)[1:n]

  # Convert the RGB values to hex codes
  shades <- grDevices::rgb(red, green, blue, maxColorValue = 255)

  return(shades)
}

#' Plot colours
#'
#' Plot a vector of colours to see what they look like
#'
#' @param colours Vector of colour to plot
#'
#' @return A ggplot2 object
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' shades <- make_shades("goldenrod", 5)
#' plot_colours(shades)
#'
plot_colours <- function(colours) {
  plot_data <- data.frame(Colour = colours)

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = .data$Colour, y = 1, fill = .data$Colour,
                               label = .data$Colour)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(angle = "90") +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
}


#' Generate random colours
#'
#' @param n amount of colours to sample
#' @param seed seed to use for random generation
#'
#' @return List of colors (hex code)
#' @export
#'
#' @examples
#' #one random colour
#' random_colors(n=1)
#'
#' #setting the seed for reproducibility
#' random_colors(n=1, seed=1234)
random_colors <- function(n = stop("No number of colours specified"), seed){

  # we want at leats one random colour
  if( n < 1 ){
    stop("N nees to be greater than 0")
  }else if( n > 657 ){
    #we onlty have 657 defaul colors
    stop("Cannot generate more than 657 colours")
  }

  if(! missing(seed)){
    set.seed(seed)
  }

  cols <- sample(grDevices::colors(),size=n)

  return(cols)

}


#' create a circle of colours
#'
#' @param colors to be plotted in a circle
#' @param n the amount of random colors to choose (will clash with colors)
#' @param lwd width of the circle line to draw
#'
#' @return the colors, that were randomly selected if n is set and no colors
#' @export
#'
#' @examples
#' #circle with 5 random colors
#' draw_color_circle(n=5)
draw_color_circle <- function(colors, n, lwd=10){

  #we need either colors or n but not both
  if(! missing(colors) && ! missing(n)){
    warning("Both colors and n was specified, will only use colors")
  }

  ret <- FALSE
  if(missing(colors) && ! missing(n)){
    colors <- random_colors(n)
    ret <- TRUE
  }

  #see how many segments we need
  segments <- length(colors)
  segmentSize <- 360/segments

  graphics::plot(-.5:2.5,-.5:2.5,type='n', axes=F, ylab="", xlab="")
  for(seg  in 1:segments){
    for(rad in seq(from=0.5,to=1, by=0.01)){
      plotrix::draw.arc(deg1 = (seg-1)*segmentSize, deg2 = seg*segmentSize, radius=rad, lwd=5, lend=2, col = colors[seg])
    }

  }

  if(ret){
    return(invisible(colors))
  }
}
