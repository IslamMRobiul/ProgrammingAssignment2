## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  sd <- NULL     ## Initializing Standard Deviation as NULL
  set <- function(y)
    x <<- y
    sd <<- NULL
    get <- function() x     #Function to get matrix x
    setsd <- function(sd) sd <<- StandardDeviation
    getsd <- function() sd
    list(
      set = set,
      get = get,
      setsd = setsd,
      getsd = getsd
    )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {    ## gets cache data
  sd <- x$getsd()        
  if (!is.null(sd)) {      ## Checking whether the Standard Deviation is Null
    message("getting cached data")
    return(sd)         ## Returns Standard Deviation Value
  }      
  data <- x$get()
  sd <- solve(data, ...)        ## Calculate Standard Deviation value
  x$setsd(sd)
  sd            ## Return a matrix that is the Standard Deviation of 'x'
}
