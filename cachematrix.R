## Pair of functions that cache the invers of a matrix to avoi repeated 
## calculation.


## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse

## Usage
## VarName<-MakeCacheMatrix(): establishes object VarName with 4 functions
##   VarName$set(matrix): sets the value of the VarName matrix
##   VarName$get: gets the value of the matrix
##   VarName$setInverse(matrix): sets the passed matrix as the inverse
##   VarName$getInverse(): returns the inverse of the VarName matrix
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     ## Creates set function
     set <- function(y) {
          #writes the incoming matrix into x
          x <<- y
          #reset m to null when new value introduced, to allow recalc of cache
          m <<- NULL
     }
     
     #Creates get function, retruns the matrix x when called
     get <- function() x

     #Create setInverse function
     setInverse <- function(solve) m <<- solve
     
     #Creates getInverse function, retruns the matrix m when called
     getInverse <- function() m
     
     # returns the list of functionto the assigned object 
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve(makeCacheMatrix Object) 
## Returns the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## Else, it calculates inverse and stores in the makeCacheMatrix object

## Usage
## VarName<-MakeCacheMatrix(): establishes object VarName with 4 functions
## VarName$set(matrix)
## cacheSolve(VarName): calculates the inverse or retrieve from cache; sets 
##   the inverse in VarName if calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     
     ## Deternmine if inverse has been calculated for current value of x
     ## If calculated m is populated and retrieve from cache and return
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## If m is NULL, then the inverse of current x must be calculated
     ## Read in x
     data <- x$get()
     ## Calculate inverse
     m <- solve(data, ...)
     ## Set inverse in x
     x$setInverse(m)
     ## Return the inverse
     m
     }
