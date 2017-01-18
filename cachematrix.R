## A code to inverse and Cache inversed matrices
## (1) makeCacheMatrix() - creates an oject containing an original input matrix to be inversed
##     as well as a list of functions to support Cache management
## (2) cacheSolve() - checks the Cache, and retrives a cached inversed matrix if it corresponds to a matrix being passed as input
##     if Cache doesn't have a corresponding inversed matrix stored, the func computes an inverted matrix and writes it out in Cache for reuse

## description in (1) 

makeCacheMatrix <- function(x = matrix()) {

  ## handling exceptions
  if (nrow(x) != ncol(x) | any(is.na(x))) {
      message("the matrix is not invertible")
      break
    }
    else
    ## regular construction of the required functions
    {
      inv <- matrix(NA, nrow(x), ncol(x))
      
      set <- function(y) {
      x <<- y
      inv <- matrix(NA, nrow(x), ncol(x))
      }

  get <- function() x
  
  setinv <- function(inversed) inv <<- inversed
  
  getinv <- function() inv
  
  ## creating the output list
  list(set = set, get = get, setinv = setinv, getinv = getinv)

    }

}


## description in (2)

cacheSolve <- function(x, ...) {

  ## retrieving a value of inversed martix paired with one held in the special "matrix" (the input data being passed)
    inv <- x$getinv()
  
  ## checking if the retrieved instance of the inversed matrix is not an empty one, and if the case - getting cached data
  if(!all(is.na(inv))) {
    message("getting cached data")
    
    ## Return a cached matrix that is the inverse of 'x'
    return(inv)
  }
  
  data <- x$get()
  inversed <- solve(data)
  x$setinv(inversed)

  ## Return a newly computed matrix that is the inverse of 'x'  
  inversed

}
