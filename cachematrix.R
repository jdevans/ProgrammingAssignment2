
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
