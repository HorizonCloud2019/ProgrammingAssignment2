## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  
  inv_matrix <- NULL
  
  ## there are getter/setter for matrix
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  ## there are  getter/setter for matrix inverse
  getinv <- function() inv_matrix
  setinv <- function(inverse) inv_matrix <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv_matrix)) {
    message("inverse  of matrix is cached")
    return(inv_matrix)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv_matrix <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv_matrix)
  
  # return inverse of matrix
  return(inv_matrix)
}
