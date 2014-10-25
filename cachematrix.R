## Two functions that will create the inverse of a matrix
## 1. makeCachematrix
## 2. cacheSolve

## makeCacheMatrix makes a special "matrix" containing a function to
## 1. set the value of the matrix (set function)
## 2. get the value of the matrix (get function)
## 3. set the value of the inverse matrix (setinverse function)
## 4. get the value of the inverse matrix (getinverse function)

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function()x
    setinverse<-function(solve) m<<-solve
    getinverse<-function()m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the makeCacheMatrix 
## function. 
## It checks first to see if the inverse matrix is already calculated. If it has been calculated it 
## takes the inverse matrix from the cache and skips the computation. Otherwise it calculates the  
## inverse matrix and sets the value of the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
