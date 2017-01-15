## Put comments here that give an overall description of what your
## functions do

##The below functions try to cache the inverse of a matrix

## Write a short comment describing this function
#this function  accepts a matrix variable as input and sets and gets
#the value of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
m<- NULL
set<-function(y){
  x<<-y
  m<<-null
}
get<-function() x
setinverse<-function(solve) m<<-solve
getinverse<- function() m
list (set = set,get = get,
      setinverse=setinverse,
      getinverse=getinverse)
}


## Write a short comment describing this function
## Cachesolve accepts a matrix that is created by makeCacheMatrix 
#and returns the inverse of the matrix, if the inverse of
#the matrix is available it returns from the cache
#While testing few examples most of the time i got Null value as inverse
#As suggested by Len in forums I am calculating determinant of matrix
#to determine to calculate inverse or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Calculate det of matrix
  if (det(x$get())!=0){
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  inverse<-x$get()
  m<-solve(inverse)
  x$setinverse(m)
  m
  }
  else{
    message("Inverse of the matrix cannot be calculated")
  }
}
