## The following functions will create special matrix func. that can 
## cache it's inverse. Compute the inverse of a matrix. If the
## inverse already exists, it will retrieve the inv. from the cache. 

## makeCacheMatrix function will:
# Set value of square matrix
# Get value of matrix
# Set the value of the inverse matrix
# Get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # upon initial run, set inverse matrix. Once set, use again to get rev.
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function will:
# computes the inverse of the above spec. matrix
# If already reversed, get the inverse from the cache (func. above)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #m <- x$getinverse()
  m <- x$getinverse()
  # if inverse already calc., get inverse from cache. 
  if(!is.null(m)) {
    message("Getting cached inversed matrix")
    return(m)
  }
  # get the matrix
  data <- x$get()
  # Calculate the inverse matrix
  message("Getting the inverse matrix...")
  m <- solve(data, ...)
  x$setinverse(m)
  message("The inverse matrix is: ")
  m
}
