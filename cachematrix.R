## Put comments here that give an overall description of what your
## functions do
### follows exactly example of makeVector and cachemean provided by Roger D. Peng in the Coursera R Programming class. For makeCacheMatrix it follows makeCacheMean expect substitues x is a matrix nont a numeric, setmean and getmean functions for getmatrix and setmatrix functions and substitues mean for solve (instead of mean) inside function associated with getmatrix (instead of getmean)
## The idea is save computation time associated with matrix inversion by caching the inverse of a matrix as an alternative to computing over and over. Here we assume the passed matrix is always invertible.

################## tested example #############
#> A<-matrix(round(runif(9,1,20),0),3,3)
#> A
#[,1] [,2] [,3]
#[1,]    7    1   13
#[2,]   19    8   10
#[3,]   18   15    1
#> a<- makeCacheMatrix(A)
#> b<- cacheSolve(a)
#> b
#[,1]   [,2]   [,3]
#[1,] -0.142  0.194 -0.094
#[2,]  0.161 -0.227  0.177
#[3,]  0.141 -0.087  0.037
#> b<- cacheSolve(a)
#getting cached data
#> b
#[,1]   [,2]   [,3]
#[1,] -0.142  0.194 -0.094
#[2,]  0.161 -0.227  0.177
#[3,]  0.141 -0.087  0.037
###############################################


## Write a short comment describing this function = The object of this function is to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   m<<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Write a short comment describing this function = The object of this function is to compute the inverse associated with the special "matrix" returned by makeCacheMatrix above. If the inverse has already exists for passed matrix, then cacheSolve should not calculate the inverse, but return it from the cache.
### follows exactly example of makeVector and cachemean provided by Roger D. Peng in the Coursera R Programming class. For CacheSolve m<- relates to getmatrix & x<- to setmatrix instead of getmean and if m need to calculate it calls solve(matrix,...) instead of mean(data,...) (associated with cachemean)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix, ...) 
  x$setmatrix(m)
  m
}
