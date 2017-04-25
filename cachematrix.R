## These two functions 'makeCacheMatrix' and cacheSolve allow us to store the 
## inverse of a given matrix. This allows us to save computation power by not repeating
## this quite computationally expensive calculation.

## This function creates a special matrix, that is essentially a list containing 4 functions that allow us to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## 1. A function to set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x ## 2. A function to get the value of the matrix
  
  setinv <- function(solve) m <<- solve ## 3. A function to set the value of the inverse matrix
  
  getinv <- function() m  ## 4. A function to get the value of the inverse matrix
  
  list(set = set, get = get,setinv=setinv,getinv=getinv) #output function list
}

## This function retrieves the cached inverse of our input matrix if it is available in the 
## special matrix. Otherwise calculates the inverse and stores it for retrieval next time. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) { # It first checks to see if the inverse has been calculated
    message("getting cached data")
    return(m) # If so it skips the computation and returns the inverse
  }
  data <- x$get() # otherwise it retrieves the input matrix
  m <- solve(data) # calculates the inverse
  x$setinv(m) # stores that inverse in special matrix
  m
}