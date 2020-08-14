##The code caches and computes the value of the inverse of a matrix.


##makeCacheMatrix creates a matrix object and caches the value of its inverse.

makeCacheMatrix<-function(x=matrix()){
      m<-NULL
      set<-function(y){
         x<<-y
         m<<-NULL
      }
      get<-function()x
      setinverse<-function(solve) m<<-solve
      getinverse<-function() m
      list(set=set,get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}

##cacheSolve computes the inverse of the matrix object created by makeCacheMatrix. If the inverse is already calculated, it retrieves the value.

cacheSolve <- function(x,...){
      m <- solve(x)
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
}
