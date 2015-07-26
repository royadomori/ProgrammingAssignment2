## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function does
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse
## Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## The function checks to see if the inverse has been computed. If the inverse has been calculated, it skips the
## computation. If not, it will calculate the inverse and set it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
