## Put comments here that give an overall description of what your
## functions do 

## Write a short comment describing this function
   
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(input_matrix) {
    x <<- input_matrix
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(input_inverse_matrix) inverse_matrix <<- input_inverse_matrix
  getinverse <- function() inverse_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
 
## Write a short comment describing this function
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  ## cacheSolve: This function computes the inverse of the special "matrix" returned by 
  ## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
  ## changed), then the cachesolve should retrieve the inverse from the cache.
  inverse_matrix <- x$getinverse()
  
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  
  inverse_matrix
}