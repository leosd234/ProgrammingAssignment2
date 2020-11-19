## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a spcial matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	mat1 <- NULL
	set <- function(y) {
		x <<- y
		mat1 <<- NULL	
	}
	get <- function() x
	set_inverse <- function(solve) mat1 <<- solve
	get_inverse <- function() mat1 
	list(set = set, get = get,
		set_inverse = set_inverse,
		get_inverse = get_inverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of a special 
# matrix returned by makeCacheMatrix function (defined above).
# If the cache is already calculated(and the matrix has not 
# changed) then cacheSolve retrieves the inverse of the matrix
# from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$get_inverse()
	if(!is.null(m)) {
			 message ("gettting cached data")
			 return(m)		
			}	
	data <- x$get()
	m <-solve(data, ...)
	x$set_inverse(m)
	m
}
# how to test
# source("cachematrix.R")
# a <- matrix(1:4, 2,2)
# b <- makeCacheMatrix(a)
#c<-cacheSolve(b)
#a;b;c;
# check if we can get from cache
# c1<- cacheSolve(b)
#
