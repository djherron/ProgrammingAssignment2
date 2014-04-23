# R Programming
# Week 3
# Programming Assignment 2 (for peer assessment)

# The functions below illustrate how encapsulation (a key characteristic
# of object-oriented programming) can be accomplished in R.  They show
# how, in R, one can implement objects that encapsulate data and
# behaviour (state and methods).  The context for the example involves
# matrices and the caching of their inverses.

# The makeCacheMatrix() function acts, effectively, as an object -- a
# special matrix object that can cache its own inverse.
# The makeCacheMatrix() function does two things: 1) it defines a
# class of CacheMatrix objects (and thereby equates to an OO class
# definition) and 2) it constructs instances of CacheMatrix objects
# (and thereby equates to an OO constructor method).  The list it
# returns acts as a reference (handle) to the constructed object, and
# the elements of the list name the object's methods.  Together, the
# list and its elements allow callers to invoke methods on the object.
makeCacheMatrix <- function(mat=matrix()) {
  # object attributes (in addition to the function's argument)
  mat_inv <- NULL
  
  # object methods
  get <- function() mat
  set <- function(new_mat) {mat <<- new_mat; mat_inv <<- NULL}
  getinverse <- function() mat_inv
  setinverse <- function(inv) mat_inv <<- inv
  
  # return object reference and method names
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The cacheSolve() function acts as a special version of R's solve()
# function -- a version smart enough to return a cached matrix inverse
# (if it exists) rather than automatically computing an inverse each
# time it's called.
cacheSolve <- function(cacheMat, ...) {
  mat_inv <- cacheMat$getinverse()
  # note: the '(and the matrix has not changed)' part of the
  # assignment requirements is satisfied by the set() method of the
  # makeCacheMatrix() object since set() ensures that when the matrix
  # changes the inverse is set to NULL, which causes the following
  # 'if' condition to resolve to 'false' thus forcing a calculation
  # of the matrix inverse.
  if(!is.null(mat_inv)) {
    message("getting cached matrix inverse")
    return(mat_inv)
  }
  mat <- cacheMat$get()
  mat_inv <- solve(mat, ...)
  cacheMat$setinverse(mat_inv)
  mat_inv
}

# Here's an example of creating and using a CacheMatrix object and
# the cacheSolve() function.

# A <- matrix(1:4,2,2)
# cm <- makeCacheMatrix(A)
# cm$get()                  
# cm$getinverse()
# cm$setinverse(solve(A))
# cm$getinverse()
# cacheSolve(cm)
# cm$set(A)
# cm$getinverse()
# cacheSolve(cm)
