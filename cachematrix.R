## The cachematrix.R file contains two functions: makeCacheMatrix() and cacheSolve().
## The makeCacheMatrix() creates a R object that stores a square matrix and its inverse.
## The cacheSolve() requires an argument that is returned by makeCacheMatrix() in order
## to retrieve the inverse from the cached value that is stored in the makeCacheMatrix()'s
## object environment.

## This function leverages on the advantage of lexical scoping in R programming. 
## With the function that return objects of type list(), 
## it allows access to any other objects defined in the environment of the original function,
## such as the values of x and inv through the use of getters and setters.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  setinv<-function(solve) inv<<-solve
  getinv<-function() inv
  list(set = set,get=get,
       
       setinv=setinv,
       
       getinv=getinv)
}


## This function attempts to retrieve the inverse from the input argument of type 
## makeCacheMatrix, i.e. a square matrix. If it is a new input argument (a new square matrix), 
## it is able to calculate and store its inverse. If it is not a new square matrix,
## (i.e. we have previously input this square matrix to find its inverse), it will return
## the cached inverse to the parent environment.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  sqmatrix<-x$get()
  inv<- solve(sqmatrix, ...)
  x$setinv(inv)
  
  inv  
  
  
}
