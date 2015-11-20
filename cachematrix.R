## This function caches the inverse of a matrix to preserve computation 
## makeCacheMatrix computes the inverse of a special matrix and stores it in the cache
makeCacheMatrix <- function(x=matricx()){
        m <-null
        set <-function(y){
                x <<- y
                m <<- NULL
        }
        setinverse <-function(inverse) m<<-inverse
        getinverse <-function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## cacheSolve returns a matrix from the cache that is an inverse of 'x'
cacheSolve <- function(x, ...){
        m <-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-inverse(data, ...)
        x$setinverse(m)
        m
}