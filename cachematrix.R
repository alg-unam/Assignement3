## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#'makeCacheMatrix'  creates a list containing a function to set/get
#value of the matrix and get/set the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	   m <- NULL
	   set <- function(y) {
	       x <<- y   
	       m <<- NULL
	   }
	   get <- function() x
	   setinverse <- function(solve) m <<- solve
	   getinverse <- function() m
	   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## 'cacheSolve' calculates the inverse of a matrix using the special "vector"
#created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the matrix using solve and sets the value of the inverse in the cache
#via the `setinverse` function.

cacheSolve <- function(x, ...) {
	  m <- x$getinverse()
	  if (!is.null(m)) {
	     message("getting cached matrix")
	     return(m)
	  }
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setinverse(m)
	  m    ## Return a matrix that is the inverse of 'x'
}

