# There are two primary functions involved in this assignment.
# The first creates an R object consisting of a list each member
# of which is a function for working with the matrix passed to it.
# The second works with those functions passed to it to return the 
# inverse of the original matrix or the cached inverse if that already exists.

# Typical use:
# Run the code for both functions, then
# create a matrix:   my_matrix <- matrix(c(10,20,30,40),2,2)
#                    z <- makeCacheMatrix(my_matrix)
#                    cacheSolve(z)   #this will then print the inverse indicating it's 'initial'
#                    cacheSolve(z)   #this will print the inverse again indicating it's 'cached'
# 

# Function description:
# 'set' is used to initialize x to the matrix
# 'get' retrieves the matrix
# 'setinv' sets the cached value of the passed matrix inverse
# 'getinv' retrieves the inverse
# These functions are assembled into a list which is passed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrse) inv <<- invrse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# Function description:
# This function takes the passed object created by makeCacheMatrix
# and, if the inverse has already been created, returns the cached value;
# otherwise, it gets the original matrix, uses 'solve' to get its
# inverse, and returns that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  message("computing inverse initially")
  x$setinv(inv)
  inv
}

