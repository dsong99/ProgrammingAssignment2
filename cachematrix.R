## Caching Matrix inverse 
## Matrix inverse calculation is an expensive operation, caching inverse result to avoid repeat calculations. 

## This function is a cache that reservs a matrix inverse result.  
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  
  get <- function(){
    x
  }
  
  getInverse <- function(){
      inv
  }
  
  setInverse <- function(inv_result){
    inv <<- inv_result
  }
  
  list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}


## This function return the passed in Matrix's inverse if its inverse it ready; 
## otherwise calculate the inverse, store the result in makeCacheMatrix object, and then return the inverse.  
## The passed in param must be a obejct of makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  
  inv <- x$getInverse()
  print(inv)
  if (!is.null(inv)){
    message('getting cached inverse')
    return(inv)
  }

  #get the matrix 
  m <- x$get()
  
  #calculate the inverse
  inv<-solve(m, ...)
  
  #store the inverse to the passed in makeCacheMatrix object
  x$setInverse(inv)
  
  #return the inverse
  inv
  
}

##test cases:
if (FALSE) {
m <- matrix(1:4, 2,2)
mc<-makeCacheMatrix(m)
cacheSolve(mc)
mc$getInverse()

m2=matrix(c(0,1,-3, -3,-4,4, -2,-2,1), 3,3)
mc$set(m2)
cacheSolve(mc)
mc$getInverse()
}