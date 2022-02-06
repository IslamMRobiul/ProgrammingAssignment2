## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix, makeCachematrix
##makeCacheMatrix consists of set, get, setinv, getinv
##library(MASS) isused to calculate inverse for non squared as well as squares matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL     # Initializing inverse as NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
    }
    get <- function() x     #function to get matrix x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() {
              inver <- ginv(x)
              inver%*%x   #function to obtain inverse of the matrix
      }
    list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)

}


## Write a short comment describing this function
## This is used to get Cache data
cacheSolve <- function(x, ...) {    # gets cache data
  inv <- x$getinv()        
  if (!is.null(inv)) {                      # Checking whether inverse is Null
                      message("getting cached data!")
                      return(inv)           # Returns inverse Value
  }      
  data <- x$get()
  inv <- solve(data, ...)        # Calculates inverse value
  x$setinv(inv)
  inv           ## Return a matrix that is the inverse of 'x'
}
