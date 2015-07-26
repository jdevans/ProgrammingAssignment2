## This pair of functions (makeCacheMatrix and cacheSolve) allow
## for the result of a matrix inversion (a potentially lengthy computation)
## to be cached and only recomputed if necessary. 
##
## Example usage:
## > library(datasets)
## > data(mtcars)
## > m<-makeCacheMatrix(mtcars[1:6,1:6])
## > cacheSolve(m)
## Mazda RX4 Mazda RX4 Wag  Datsun 710 Hornet 4 Drive Hornet Sportabout     Valiant
## mpg    1.6912172    -2.1246087  0.28373377    -0.35946411      -0.073188537  0.70128447
## cyl    4.4779580    -4.5193481 -0.13701853    -0.88840025      -0.277132065  1.55731209
## disp  -0.0959297     0.1149989 -0.01688931     0.04206857       0.002364724 -0.05303118
## hp     0.1097871    -0.1599798  0.04452973    -0.07290840       0.016894914  0.07088814
## drat -12.2658012    15.5529033 -1.88007166     3.63284633       0.246911606 -5.99578277
## wt    -3.9215686     3.9215686  0.00000000     0.00000000       0.000000000  0.00000000


## The first function, makeCacheMatrix, creates a "pseudo-matrix", 
## which is really a list containing the following four functions:
##
## set(): set the matrix
## get(): get the matrix
## setInverse(): cache the inverse matrix
## getInverse(): retrieve the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                 # A fresh matrix: reset any cached inverse
  set <- function(y) {        # Define set(): note use of the <<- operator
    x <<- y                   # to assign a value to an object in an
    inv <<- NULL              # environment different from the current one.
  }
  get <- function() x           # Define get(): Fetch matrix
  setInverse <- function(solve) # Define setInverse(): cache inverse matrix
    inv <<- solve               # (Note 2nd use of the <<- operator)
  getInverse <- function() inv  # Define getInservse(): fetch cached inverse
  list(set = set, get = get,    # Return the list of functions
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function, cacheSolve, accepts a "pseudo-matrix" produced
## by makeCacheMatrix and tries to fetch any cached inverse matrix.
## If the retrieved inverse matrix is null, it computes the inverse 
## and caches it using the "pseudo-matrix."

cacheSolve <- function(x, ...) {   # Receive a "pseudo-matrix"
  inv <- x$getInverse()            # Try to fetch a cached inverse matrix
  if(!is.null(inv)) {              # If we find one
    message("getting cached data") # print a victorious message
    return(inv)                    # and return it 
  }
  data <- x$get()                  # Otherwise, fetch the matrix
  inv <- solve(data, ...)          # Invert it
  x$setInverse(inv)                # Cache the result
  inv                              # And return it
}
