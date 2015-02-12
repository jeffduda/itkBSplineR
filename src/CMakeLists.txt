cmake_minimum_required(VERSION 2.8.7)
project(itkBSplineR_SuperBuild)
include( ExternalProject )
# if 'CMAKE_BUILD_TYPE' is not specified by user, assume RelWithDebInfo
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE RelWithDebInfo )
endif( NOT CMAKE_BUILD_TYPE )

# Note: Chaning the GIT_TAG (i.e changing ITK version), may require
#  an update to the ITK_INCLUDE variable in src/Makevars
#  as the path includes the ITK version (e.g ${INSTALL_DIR}/include/ITK-4.7/)

# HACK This code fixes a loony problem with HDF5 -- it doesn't
#      link properly if -fopenmp is used.
string(REPLACE "-fopenmp" "" ITK_CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
string(REPLACE "-fopenmp" "" ITK_CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

ExternalProject_Add(ITK
  GIT_REPOSITORY https://github.com/InsightSoftwareConsortium/ITK.git
  GIT_TAG 6e55ef004c197d0e0850c9a65fe553c4156d3bcf
  DOWNLOAD_DIR ${itkBSplineR_SuperBuild_SOURCE_DIR}/ITK-src
  SOURCE_DIR ${itkBSplineR_SuperBuild_SOURCE_DIR}/ITK-src
  BINARY_DIR ${itkBSplineR_SuperBuild_SOURCE_DIR}/ITK-build
  INSTALL_DIR ${itkBSplineR_SuperBuild_SOURCE_DIR}/ITK-install
    CMAKE_ARGS
    -Wno-dev
    --no-warn-unused-cli
    -DBUILD_EXAMPLES:BOOL=OFF
    -DBUILD_SHARED_LIBS:BOOL=OFF
    -DBUILD_TESTING:BOOL=OFF
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DITK_LEGACY_REMOVE:BOOL=OFF
    -DITK_FUTURE_LEGACY_REMOVE:BOOL=ON
    -DITKV3_COMPATIBILITY:BOOL=ON
    -DITK_BUILD_DEFAULT_MODULES:BOOL=ON
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_CXX_FLAGS="-fPIC"
    -DCMAKE_INSTALL_PREFIX=${itkBSplineR_SuperBuild_SOURCE_DIR}/ITK-install
  cmakedefine NDEBUG
)

set( itkBSplineR_DEPENDSON_ITK ITK ) # set so ITK is built prior to package
