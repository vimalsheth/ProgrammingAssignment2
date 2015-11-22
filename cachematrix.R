# makeCacheMatrix stores a matrix X in memory
# m is supposed to be inverse of the matrix, to be retrieved through getinverse() function


makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  set<-function(y)
  {
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

#CacheSolve function looks for the 'cached' version of the inverse matrix if exists; else inverses the member and caches the same
cacheSolve <- function(x=matrix(), ...) {
  
  m<-x$getinverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  datos<-x$get()
  
  m<-solve(datos, ...)
  
  x$setinverse(m)
  
  m
}
