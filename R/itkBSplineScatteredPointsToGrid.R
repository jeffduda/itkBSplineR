#' Generate a regular grid of data from scattered points
#'
#' Generate a regular grid of data from scattered points using BSplines
#'
#'
#' @param x matrix of input parameter space values
#' @param y matrix of input data space values
#' @param origin vector of parameter space starting point
#' @param spacing vector of parameter spacing between points in grid
#' @param size vector of size of output matrix
#' @return A matrix of regularly space data values

#' @author Duda JT
#' @examples
#'
#' \dontrun{
#' t = as.matrix(seq(from=0.0, to=1.0+1e-10, by=0.05))
#' V0 = 0.25 * cos(t*6.0*3.141)
#' V1 = 0.25 * sin(t*6.0*3.141)
#' V2 = 4.00 * t
#' V = cbind(V0,V1,V2)
#' VGrid = itkBSplineScatteredPointsToGrid(t,V,origin=0, size=101, spacing=0.01)
#' }
#'
#' @export itkBSplineScatteredPointsToGrid


itkBSplineScatteredPointsToGrid <- function( x, y, origin=NA, spacing=NA, size=NA )
{

  if ( !is.matrix(x) || !is.matrix(y) ) {
    stop("Inputs x and y must be matrices")
  }

  spacing = as.vector( spacing )
  size = as.vector( size )
  origin = as.vector( origin )

  oList =  .Call("itkBSplineScatteredPointsToGrid", x, y, size, spacing, origin, PACKAGE = "itkBSplineR")

  names(oList) = paste("V", seq(1:length(oList)), sep='')
  return(oList)

}
