## Two functions wherein one (makeCacheMatrix) creates a matrix that can cache its inverse and
## another (cacheSolve) computes the inverse of the matrix from the first function

## Creates a matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     ##setting up the inverse as NULL
  set <- function(y){  ##setting the function
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}   ##the function that creates the x matrix, matrix that caches its inverse
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This will compute the inverse of the matrix returned from the first function,
## makeCacheMatrix

cacheSolve <- function(x, ...) {    ##gathers the cahed data
  inv <- x$getInverse()
  if(!is.null(inv)){        ##checks if the data is inverse and can be identified as NULL
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)  ##computes for the inverse value
  x$setInverse(inv)
  inv
              ## Return a matrix that is the inverse of 'x'
}
