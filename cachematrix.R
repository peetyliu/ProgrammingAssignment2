## This function creates a special vector which is a list a functions to do

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## The following function calculates the inverse of the special vector
## created with "makeCacheMatrix" function. It will first check if the 
## inverse has already been calculated. If so, it will get the inverse
## from the cache and skip the calculation.
## otherwise, it will calculate the inverse of the matrix and se the 
## value in cache.

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}
