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
