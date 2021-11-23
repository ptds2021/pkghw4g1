#' @title Estimate area function
#'
#' @authors Corinne & Laurène
#'
#' @param x A non-null positive integer
#'
#' @return Print a list with the area estimation and the points
#'
#' @example estimate_area(100)
#'
#' @export
estimate_area <- function(B = 5000, seed = 10){

  if(!is.numeric(B) | B <= 0){  # TO MODIFY
    stop("B must be a non-null positive integer") # TO MODIFY
  }

  if(is.matrix(B)){ #to check whether user inputs a matrix for B
    stop("B must be a non-null positive integer") #stop the function and return message
  }

  if(!is.numeric(seed)){ #to check whether user inputs a correct value for the seed
    stop("The seed must be an integer or NULL") #stop the function and return message
  }


  set.seed(seed)         # to reproduce the example

  # Points simulation
  points <- data.frame(x = runif(n = B, min = 0, max = 1),
                       y = runif(n = B, min = 0, max = 1),
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
#' @authors Corinne & Laurène
#'
#' @param x a list provided by estimate_area() function
#'
#' @return plot the estimated area and its points
#'
#' @example
#' rval <- estimate_area(100)
#' plot.area(rval)
#'
#' @export
plot.area <- function(x) {

  if(class(x) != "area"){
    stop("X has to be of class area for this function.")
  }

  points <- x[["points"]]
  B <- nrow(points)

  # plot points

  cols = c('#ffadad33',  # light red, 33 to add transparency
           '#4cc2c233')  # blue

  # Plot graph with square and shape S
  par(pin=c(3.5,3.5)) # to have a square plot
  plot(NA,
       xlim = c(-0.1,1.1), # set x and y axis
       ylim = c(-0.1,1.1),
       xlab = "x",
       ylab = "y")
  make_square()       # plot square with created function
  grid()
  for (i in 1:B){
    points(points[i,1], points[i,2], pch=20, col=cols[1 + (points[i,3])])

  }

  make_shape()        # plot shape with created function
  # add grid on the plot
}
