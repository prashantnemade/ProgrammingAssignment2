## makeCacheMatrix is creating list which stores and sets matrix and its inverse. cacheSolve returns inverse of a matrix.

## makeCacheMatrix function creates a list of 4 functions. Below are the details of each function,
## 1. set - This function is used to pass the matrix for which we want to get inverse
## 2. get - This function is used to retrieve the matrix for which we want to get inverse
## 3. setinverse - This function is assigning inverse of a matrix
## 4. getinverse - This function is retrieving the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve is the function which returns the inverse of a matrix. If the inverse of a matrix is not 
## available then it will compute the inverse of a matrix and return it. If the inverse of a matrix has already been computed then it will retrieve it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
