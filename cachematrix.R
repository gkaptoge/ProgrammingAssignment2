## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and cache's its inverse
## You do not need to calculate the matrix over and over again
## e.g. in a loop you can just lookup the inverse

## makeCacheMatrix is a function takes any matrix 
## It then creates a list of 4 functions (set,get,setinv,getinv)
## set function replaces a new matrix (y) with existing matrix(x)
## The set function also sets the matrix inverse (m) to NULL
## The get function simply returns the matrix
## The setinverse function sets the inverse of the matrix
## The getinverse gets the inverse of the matrix

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
