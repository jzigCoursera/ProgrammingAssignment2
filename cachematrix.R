## Function: makeCacheMatrix
## this function takes a numeric matrix as an input and has 4 internal functions
## set, store a variable
## get, return the variable
## setInv, store the inverse of the input matrix
## getInv, return the stored inverse set in setInv
makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list (set = set, get = get, 
          setInv = setInv, 
          getInv = getInv) 
}

## Function: cacheSolve
## expects makeCacheMatrix as an input, with a numeric matrix as a passed var
## retrieves the value of the stored matrix, checks if the value is null, 
## if not null, the stored value is returned
## else the value of the matrix is retrieved and the inverse calculated &&
## and stroed
cacheSolve <- function(x, ...) {
      m < x$getInv()
      if(!is.null(m)) {
          message("getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInv(m)
      m
}
