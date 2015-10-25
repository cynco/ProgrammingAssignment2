## This function creates a special "matrix" object that can cache its inverse.

## x is a numeric square matrix
## makeCacheMatrix returns a list containing a function that contains #1-4 described below.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL  #this will be the inverse matrix
	## 1. A function to cache the value of the matrix
	set<- function(y) {
		x<<-y
		m<<-NULL
	}
	## 2. A function to retrieve the matrix 
	get<-function() x
	## 3. A function to solve and cache the inverse matrix
	setinverse<-function(solve) m<<-solve
	## 4. A function to retrieve from cache the value of the inverse.
	getinverse<-function() m
	##Return the list of four functions
	list(set=set, get=get,
	     setinverse=setinverse,
	     getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

## x is a numeric square matrix
## cacheSolve returns the inverse of 'x'

cacheSolve <- function(x, ...) {
	#Retrieve the inverse matrix 'm' from the cache.
    m<- x$getinverse()
    ##Check that m from cache has a value.
    if(!is.null(m)) {
 	     message("getting cached inverse")
 	     return(m)
    } 
    ##Otherwise, m needs to be calculated using the functions defined in makeCacheMatrix
    ##First, retrieve the original matrix x
    data<- x$get()
    ##Solve for the inverse matrix
    m<-solve(data, ...)
    ##Cache the value of the inverse
    x$setinverse(m)
    ##Return the inverse
    m
}
