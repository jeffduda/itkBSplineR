#include <exception>
#include <vector>
#include <string>
#include <Rcpp.h>
#include "itkVector.h"
#include "itkPointSet.h"
#include "itkImage.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"

template< unsigned int SplineDimension, unsigned int DataDimension >
SEXP itkBSplineScatteredPointsToGridHelper( SEXP r_x,
  SEXP r_y, SEXP r_size, SEXP r_spacing, SEXP r_origin )
{
  Rcpp::Rcout << "In helper with spline dim: " << SplineDimension << std::endl;
  Rcpp::Rcout << " and data dim:             " << DataDimension << std::endl;

  Rcpp::NumericMatrix xMat( r_x );
  Rcpp::NumericMatrix yMat( r_y );
  Rcpp::NumericVector isize( r_size );
  Rcpp::NumericVector ispacing( r_spacing );
  Rcpp::NumericVector iorigin( r_origin );

  unsigned int nPoints = xMat.nrow();

  typedef double                                        PixelType;
  typedef itk::Image<PixelType, SplineDimension>        InputImageType;
  typedef double                                        RealType;
  typedef itk::Vector<RealType, DataDimension>          VectorType;
  typedef itk::Image<VectorType, SplineDimension>       VectorImageType;
  typedef VectorImageType ImageType;

  typedef itk::PointSet
    <typename VectorImageType::PixelType, SplineDimension>   PointSetType;

  typename InputImageType::Pointer inputImage = InputImageType::New();
  typename PointSetType::Pointer pointSet = PointSetType::New();

  Rcpp::Rcout << "Input values" << std::endl;
  for( unsigned int i=0; i<nPoints; i++)
  {
    typename PointSetType::PointType point;
    typename PointSetType::PixelType pointData( DataDimension );

    for (unsigned int p=0; p<SplineDimension; p++)
    {
      point[p] = xMat(i,p);
    }

    for (unsigned int d=0; d<DataDimension; d++)
    {
      pointData[d] = yMat(i,d);
    }

    pointSet->SetPoint( i, point );
    pointSet->SetPointData( i, pointData );

    Rcpp::Rcout << "Set point: " << i << " = " << point << " -> " << pointData << std::endl;

  }

  // Instantiate the filter and set the parameters
  typedef itk::BSplineScatteredDataPointSetToImageFilter
    <PointSetType, ImageType> FilterType;
  typename FilterType::Pointer filter = FilterType::New();

  // Define the parametric domain
  typename ImageType::SpacingType spacing;
  typename ImageType::SizeType size;
  typename ImageType::PointType origin;
  for (unsigned int i=0; i<SplineDimension; i++)
  {
    spacing[i] = ispacing[i];
    origin[i] = iorigin[i];
    size[i] = isize[i];
  }

  filter->SetSize( size );
  filter->SetOrigin( origin );
  filter->SetSpacing( spacing );
  filter->SetInput( pointSet );

  filter->SetSplineOrder( 3 );
  typename FilterType::ArrayType ncps;
  ncps.Fill( 4 );
  filter->SetNumberOfControlPoints( ncps );
  filter->SetNumberOfLevels( 5 );
  filter->SetGenerateOutputImage( true );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
  {
    Rcpp::Rcout << "ExceptionObject caught !" << std::endl;
    Rcpp::Rcout << err << std::endl;
    //return EXIT_FAILURE;
  }

  // Write the output
  Rcpp::Rcout << "Output values:" <<  std::endl;
  itk::ImageRegionIteratorWithIndex<VectorImageType>
    Itt( filter->GetOutput(), filter->GetOutput()->GetLargestPossibleRegion() );

  Rcpp::List outList(SplineDimension);
  for ( unsigned int b=0; b<SplineDimension; b++ )
    {

    Rcpp::NumericMatrix gridValues(size[0], DataDimension);

    for ( Itt.GoToBegin(); !Itt.IsAtEnd(); ++Itt )
      {

      for ( unsigned int i=0; i<DataDimension; i++ )
        {
        gridValues(Itt.GetIndex()[b],i) = Itt.Value()[i];
        }

      Rcpp::Rcout << Itt.GetIndex() << " - " << Itt.Value() << std::endl;
    }

    outList[b] = gridValues;
  }

  //outList[0] = xMat;
  //outList[1] = yMat;
  //outList[2] = gridValues;


  Rcpp::Rcout << "Return values" << std::endl;

 return Rcpp::wrap( outList );
}

RcppExport SEXP itkBSplineScatteredPointsToGrid( SEXP r_x, SEXP r_y,
  SEXP r_size, SEXP r_spacing, SEXP r_origin )
{



  if( r_x == NULL  )
    {
    Rcpp::Rcout << "Invalid Arguments: pass x values" << std::endl ;
    return Rcpp::wrap( 0 ) ;
    }
  if( r_y == NULL  )
    {
    Rcpp::Rcout << "Invalid Arguments: pass y values" << std::endl ;
    return Rcpp::wrap( 0 ) ;
    }
  if( r_spacing == NULL  )
    {
    Rcpp::Rcout << "Invalid Arguments: pass spacing values" << std::endl ;
    return Rcpp::wrap( 0 ) ;
    }
  if( r_size == NULL  )
    {
    Rcpp::Rcout << "Invalid Arguments: pass size values" << std::endl ;
    return Rcpp::wrap( 0 ) ;
    }
  if( r_origin == NULL  )
    {
    Rcpp::Rcout << "Invalid Arguments: pass origin values" << std::endl ;
    return Rcpp::wrap( 0 ) ;
    }

  Rcpp::NumericMatrix xMat( r_x );
  Rcpp::NumericMatrix yMat( r_y );

  Rcpp::NumericVector size( r_size );
  Rcpp::NumericVector spacing( r_spacing );
  Rcpp::NumericVector origin( r_origin );

  unsigned int dimension = xMat.ncol();
  unsigned int nXPoints = xMat.nrow();
  unsigned int nYPoints = yMat.nrow();

  if ( nXPoints != nYPoints )
  {
    Rcpp::Rcout << "x and y must have same number of points" << std::endl;
    return Rcpp::wrap(0);
  }

  unsigned int dDim = yMat.ncol();

  Rcpp::Rcout << "BSpline Dimension =" << dimension << std::endl;
  Rcpp::Rcout << "Data Dimension    =" << yMat.ncol() << std::endl;
  Rcpp::Rcout << "Output Size       =" << size[0] << std::endl;
  Rcpp::Rcout << "Output Spacing    =" << spacing[0] << std::endl;
  Rcpp::Rcout << "Output Origin     =" << origin[0] << std::endl;

  if ( dimension == 1 )
    {
      switch( dDim )
        {
        case 1: return itkBSplineScatteredPointsToGridHelper<1,1>( r_x, r_y, r_size, r_spacing, r_origin );
        break;
        case 2: return itkBSplineScatteredPointsToGridHelper<1,2>( r_x, r_y, r_size, r_spacing, r_origin );
        break;
        case 3: return itkBSplineScatteredPointsToGridHelper<1,3>( r_x, r_y, r_size, r_spacing, r_origin );
        break;
        case 4: return itkBSplineScatteredPointsToGridHelper<1,4>( r_x, r_y, r_size, r_spacing, r_origin );
        break;
        case 5: return itkBSplineScatteredPointsToGridHelper<1,5>( r_x, r_y, r_size, r_spacing, r_origin );
        break;
        case 6: return itkBSplineScatteredPointsToGridHelper<1,6>( r_x, r_y, r_size, r_spacing, r_origin );
        break;
        default:
        break;
        }
    }
  if ( dimension == 2 )
    {
    //return itkBSplineScatteredPointsToGridHelper<2>( r_x, r_y, r_xout );
    }
  else if ( dimension == 3 )
    {
    //return itkBSplineScatteredPointsToGridHelper<3>( r_x, r_y, r_xout );
    }
  else if ( dimension == 4 )
    {
    //return itkBSplineScatteredPointsToGridHelper<4>( r_x, r_y, r_xout );
    }

  else Rcpp::Rcout << " Dimension " << dimension << " is not supported " << std::endl;

  return Rcpp::wrap( 0 );
}
