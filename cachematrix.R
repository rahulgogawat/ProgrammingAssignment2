# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  #initialise a null object m which will be used to store the inverse matrix
  set <- function(y) {
    x <<- y          #sets the value of x
    m <<- NULL       #sets the value of m    
  }
  get <- function() x   #get the value of input matrix
  setinversematrix <- function(inverse) m <<- inverse     
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

#The following function calculates the inverse of the special "matrix" created
#with the above function. However, it first checks to see if the inverse has already
#been calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse
#in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
