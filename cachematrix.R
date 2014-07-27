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


cacheSolve <- function(x=matrix(), ...) {
## Return a matrix that is the inverse of 'x'
  
  ##Inv will be a matrix with one row and one column containing 'NA' if the inverse of x 
  ##has not previously been computed; otherwise, it will have a numerical value in the top left 
  ##corner. Testing for an 'NA' in the [1,1] position will indicate whether to go to the cached 
  ##value or to compute.
  Inv <- x$getInv()
  if (!is.na(Inv[1,1])) {
    message("getting cached data")
    return(Inv)
    ##stop and return the value of the inverse from cache
  }
  ##retrieve the matrix and solve for the inverse since the condition of the above if was not met
  M <- x$get()
  Inv <- solve(M)
  x$setInv(Inv)
  Inv
}

