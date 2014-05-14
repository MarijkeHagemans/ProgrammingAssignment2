## Put comments here that give an overall description of what your
## functions do:
## - set, will set the matrix to the cache
## - get, will get the matix from the cache
## - setinverse, will set the matrix inverse to the cache
## - getinverse, will get the matrix inverse from the cache
##
## Write a short comment describing this function:
## The function makeCacheMatrix is a class of functions to implement on a matrix in order to set 
## a matrix to the cache, get the set matrix, store a calculated inverse of the matrix to the cache and get the calculated 
## inverse of a matrix.
## 
## To use the makeCacheMatrix in the function cacheSolve, first instantiate the makeCacheMatrix 
## and then set the matrix in the instantiated object, ie. 
##  test_matrix <- matrix( c(2, 4, 3, 1), nrow=2, ncol=2)
##  test<-makeCacheMatrix()
##  test$set(test_matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The cacheSolve function will receive the object created after instantiation and setting the matrix 
## in the above functie makecacheSolve. 
## First the function will check if an inverse of the matrix is already stored in the cache. If so,
## the function will get the inverse from cache and return to result to the user, else the function will
## get the matrix from the cache and calculate the inverse. The inverse is stored in the cache using
## the function makechacheSolve.setinverse, and will return the result to the user

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("calculate data")
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
