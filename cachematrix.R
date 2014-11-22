## These functions will calculate the inverse of a matrix and store it in a value labeled inv.
## If the function has been previously calculated, it will retrieve and return said value to the console.
## This process saves the time it would take to revaluate the input. 


## Write a short comment describing this function
##function stores the value of the inverser matris as inv and the matrix by passing it to the global environment.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #will store inversed matrix
  #stores matrix and inv in the global environment using superassignment (<<-)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #returns original matrix. Cashesolve will call this function to get the original matrix
  setinv <- function(inverse) {inv <<- inverse} #called the first time cacheSolve is run
  getinv <- function() inv #used to try and retrieve saved inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##this function first looks if the inverse matrix has been calculated 
##previously, if so returns that value. Otherwise, it calculates the 
##inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #this section will only return a 
  inv <- x$getinv() #recovers stored inv, if available
  if(!is.null(inv)){
    message("getting cashed data") 
    return(inv)
  }
  matrix <- x$get() #gets matrix
  inv <- sapply(matrix, solve) #calculates the inverse of the matrix
  x$setinv(inv) #saves inv
  inv #returns inv (either the saved or the fresly claculated version)
}
