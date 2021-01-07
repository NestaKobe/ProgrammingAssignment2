## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {    ## define the set function to assign new 
      x <<- y               ## value of matrix in parent environment
      inv <<- NULL          ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x     ## define the get function - returns value of the matrix argument
    setInverse <- function(inverse) inv <<- inverse ## assigns value of inv in parent environment 
    getInverse <- function() inv                    ## gets the value of inv where called
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
    ## you need this in order to refer to the functions with the $ operator
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}



example <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))

example$get()
  #     [,1] [,2]
  #[1,]    1    3
  #[2,]    2    4

example$getInverse()
  #NULL

cacheSolve(example)
  #     [,1] [,2]
  #[1,]   -2  1.5
  #[2,]    1 -0.5


example$getInverse()
  #     [,1] [,2]
  #[1,]   -2  1.5
  #[2,]    1 -0.5
