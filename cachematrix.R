################################## 
# makeVector creates a special "matrix", which is really a list containing a function to
# setM: sets the value of the matrix
# getM: gets the value of the matrix
# setInv: sets the value of the inverse
# getInv: gets the value of the inverse
##################################
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setM <- function(y) {
    x <<- y
    m <<- NULL
  }
  getM <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(setM = setM, getM = getM,
       setInv = setInv,
       getInv = getInv)
}
#
#
##################################
# calculates the inverse of the special "matrix" created with makeCacheMatrix function
# first it checks to see if the inverse has already been calculated and stored
##################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getM()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
