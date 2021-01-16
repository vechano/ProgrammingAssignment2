## R Programming Week 3: Programming Assignment 2 - Lexical Scoping

## This function returns a list which contains functions to:
##    1. set the matrix - sets the required matrix
##    1. get the matrix - returns the given matrix 
##    1. set the inverse of the matrix - sets the inverse of the given matrix in the cache after calculation
##    1. get the inverse of the matrix - gets the inverse of the given matrix from the cache

makeCacheMatrix <- function(x = matrix(),y) {
## x currently is a 1x1 matrix containing nothing
  
  inv = NULL
  set = function(y) {
    ## set takes the matrix to be inverted as an argument
    ## use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function returns either the cached inverse of the matrix or solves the inverse if it is
# not available in the cache

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

