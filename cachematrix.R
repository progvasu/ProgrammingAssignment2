# ------------------------------------------------------------------------------ #

### Description ->

# Following pair of functions "cache's the Inverse of a Matrix".

# Matrix inversion is usually a costly computation and hence it is beneficial to cache
# the inverse of a matrix rather than computing it repeatedly.

# The function "makeCacheMatrix" creates a special "matrix" object.
# The special "matrix" is a list containing the following functions -> 
# 1. set the matrix.
# 2. get the matrix.
# 3. set the inverse of the matrix.
# 4. get the inverse of the matrix.

# The function "cacheSolve" computes the inverse of the special "matrix" returned by 
# "makeCacheMatrix". If the inverse has already been calculated (and the matrix has not 
# changed), then the "cacheSolve" retrieve's the inverse from the cache.

# NOTE - It is assumed that the matrix supplied is always invertible.

# ------------------------------------------------------------------------------ #


### "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
   
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialising the inverse_matrix variable.
  inverse_matrix <- NULL
  
  # "set" function sets the value of the input_matrix and
  # does a cache dump if the matrix is changed.
  set <- function(input_matrix) {
    x <<- input_matrix
    inverse_matrix <<- NULL
  }
  
  # "get" function returns the matrix.
  get <- function() x
  
  # "setinverse" function sets the value of the inverse of the matrix.
  setinverse <- function(input_inverse_matrix) inverse_matrix <<- input_inverse_matrix
  
  # "getinverse" function returns the value of the inverse of the matrix.
  getinverse <- function() inverse_matrix
  
  # returning the special "matrix" object as a list.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
 

### "cacheSolve" function returns the matrix that is the inverse of 'x'.
 
cacheSolve <- function(x, ...) {
  
  # getting the value of the inverse of input matrix "x" even if it is NULL.
  inverse_matrix <- x$getinverse()
  
  # checking if the value of the inverse matrix is NULL or not.
  # if the value returned is not NULL then the inverse of the matrix is already cached
  # and that value is returned (by the following if block).
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  
  # if the inverse is not already cached the following statements will calculate the inverse
  # and set the value of inverse in cache.
  
  # getting the data of the input matrix.
  data <- x$get()
  
  # calculating inverse using solve() function.
  inverse_matrix <- solve(data, ...)
  
  # caching the value of the computed inverse.
  x$setinverse(inverse_matrix)
  
  # returning the value of the computed inverse.
  inverse_matrix
}


### NOTE ->
# The check for the change of the value of the matrix is done in the "makeCacheMatrix"'s set() 
# function.
# Whenever the value of the matrix is changed (which is only possible by calling set() function
# of the "makeCacheMatrix") "cache dump" occurs ie. the value of inverse matrix stored is dumped 
# (made NULL).