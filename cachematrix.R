## This program executes uses two functions to calcuate and store
## a function result in cache memory
## The first function takes a squared matrix as input
## and builds four functions that it stores in alist
## The second function will check to see if the inverse has been calculated
## and stored in memory. If it has, it will retrive it from memory.
## If not, it will calculate it, and store it in memory.

#Creates a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x=matrix()){
    i<-NULL
    set<- function(y) {
        x<<- y
        i<<-NULL
    }
    get <- function() x
    setinverse<-function(solve) i<<- solve
    getinverse<-function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#Computes the inverse of the special "matrix"  returned by makeCacheMatrix above.
# If the inverse has already been calculated ( and the matrix has not been changed), then the cachesolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...){
    i<-x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i <-solve(data, ...)
    x$setinverse(i)
    i
}