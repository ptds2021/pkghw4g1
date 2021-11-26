#' @title Estimate area function
#'
#' @author Corinne & Laurene
#'
#' @param B a non-null positive integer
#' @param seed an integer or NULL
#'
#' @return Print a list with the area estimation and its points
#'
#' @examples
#' estimate_area(100)
#'
#' @export
estimate_area <- function(B = 5000, seed = 10){

  if(B%%1 !=0 | B <= 0){
    stop("B must be a non-null positive integer")
  }

  if(is.matrix(B)){ #to check whether user inputs a matrix for B
    stop("B must be a non-null positive integer") #stop the function and return message
  }

  if(seed %%1 !=0){ #to check whether user inputs a correct value for the seed
    stop("The seed must be an integer or NULL") #stop the function and return message
  }


  set.seed(seed)         # to reproduce the example

  # Points simulation
  points <- data.frame(x = stats::runif(n = B, min = 0, max = 1),
                       y = stats::runif(n = B, min = 0, max = 1),
                       inside = rep(NA, B)    # logical column: T if point inside shape, F if not
  )

  # point <- matrix(runif(2*B, 0, 1), B, 2)   # simulation of B coordinates
  #  Z <- matrix(rep(NA, B), B, 1)             # creation of empty vector
  for (i in 1:B){
    x <- points[i,1]
    y <- points[i,2]
    if (x^2 + y^2 > 0.5^2 &           # D1
        (x-0.5)^2 +(y-0.5)^2 < 0.5^2 & # D2
        y > x - 0.5 ){                # D3
      points[i,3] = TRUE
    }else{
      points[i,3] = FALSE
    }
  }

  estimated_area = sum(points$inside)/B  #return shape S area

  # create a structure
  rval <- structure(
    list(
      estimated_area = estimated_area,
      points = points),
    class = "area"
  )

  # return rval
  return(rval)

}


#' @title Plot area function
#'
#' @author Corinne & Laurene
#'
#' @param x a list provided by estimate_area() function
#'
#' @return Return a plot of the estimated area and its points
#'
#' @examples
#'x <- estimate_area(100)
#' plot_area(x)
#'
#' @export
plot_area <- function(x) {

  if(class(x) != "area"){
    stop("X has to be of class area for this function.")
  }

  points <- x[["points"]]
  B <- nrow(points)

  # plot points

  cols = c('#ffadad33',  # light red, 33 to add transparency
           '#4cc2c233')  # blue

  # Plot graph with square and shape S
  graphics::par(pin=c(3,3)) # to have a square plot
  plot(NA,
       xlim = c(-0.1,1.1), # set x and y axis
       ylim = c(-0.1,1.1),
       xlab = "x",
       ylab = "y")
  ### create a function to draw square
  make_square <- function() {
    graphics::rect(0,0,1,1,              # set positions of: xleft, ybottom, xright, ytop
         border = "darkblue")
  }
  make_square()       # plot square with created function
  graphics::grid()
  for (i in 1:B){
    points(points[i,1], points[i,2], pch=20, col=cols[1 + (points[i,3])])

  }
  ### create a function to draw shape S
  make_shape = function(bottom = c(0.5,0), col = "darkblue", fill = NULL){
    x <- 1
    y <- 1
    graphics::lines(bottom + 0.5, bottom, col = col) # D3: y > x - 0.5
    graphics::curve(expr = (sqrt(-(x^2) + 0.5^2)), # D1: (x^2)+(y^2)>0.5^2
          from = c(0, 0.5), #from & to are to limit the area of the curve to the rectangle
          to = c(1, 0.5),
          col = col, #set color of the curve
          add = TRUE) #curve should be added to the existing plot
    graphics::curve(expr = (sqrt(0.5^2 - (x-0.5)^2) + 0.5), # D2: (x-0.5)^2 +(y-0.5)^2) < 0.5^2
          from = c(0, 0.5), #from & to are to limit the area of the curve to the rectangle
          to = c(1, 0.5),
          col = col, #set color of the curve
          add = TRUE) #curve should be added to the existing plot
  }
  make_shape()        # plot shape with created function
  # add grid on the plot
}
