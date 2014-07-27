 ##This function creates  special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  
  Inv <- matrix() #set up empty matrix to hold inverse
  
  #define x and Inv so that they can be referenced outside of this environment
  set <- function(J){
        x <<- J
        Inv <<- matrix()
        }
  
  #store the original matrix
  get <- function() x
  
  #define setInv so that it can be referenced outside of this environment
  setInv <-function(solve) Inv <<- solve
  
  #store the inverse of the matrix
  getInv <-function() Inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## This function takes a matrix, checks to see if its inverse has already been computed 
##and cached--if so, that is returned without another computation; if not, the computation 
##is made and returned.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
          
  Inv <- x$getInv()
  if (!is.na(Inv[1,1])) {
    message("getting cached data")
    return(Inv)
  }
  M <- x$get()
  Inv <- solve(M)
  x$setInv(Inv)
  Inv
}
}
