## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initialize an empty matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get matrix
  
  setInverse <- function(inverse) m <<- inverse # set values 
  getInverse <- function() m # get values 
  
  list (set = set, get = get,
        setInverse = setInverse, getInverse = getInverse) # provides the list of above functions
}


## cacheSolve: Computes the inverse of the special "matrix" returned by makeCachematrix
# IF the inverse has already been calculated, then retreive the inverse from the cache

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  originalMatrix <- x$get() # inverse has not been calculated
  inverseMatrix <- solve(originalMatrix) # compute the inverse
  
  x$setInverse(inverseMatrix)
  inverseMatrix
}

# test
x <- matrix(1:4, nrow = 2, ncol = 2)
x
special_x <- makeCacheMatrix(x)
cacheSolve(special_x)

