## Compute Matrix Inverse (for invertible square matrices)
##  * Efficient for repeated inputs through caching Inputs & computed data

## Creates a special matrix object with functions to Cache Matrix & Inverse

makeCacheMatrix <- function(x = matrix()) {
	mi <- NULL						# Initialize Null value for Inverse
	setm <- function(y){					# f(x) for new/changed matrix:
		x <<- y							# Pass Matrix Value to Cache
		mi <<- NULL						# & Set Null Value for Inverse
	}
	getm <- function() x					# f(x): Pass Matrix value to cache
	setmi <- function(sol) mi <<- sol			# f(x): Pass Inverse value to Cache
	getmi <- function() mi					# f(x): Get Inverse Value from Cache
	list(setm=setm, getm=getm,				# House the functions in a list
		 setmi=setmi,
		 getmi=getmi)
}


## Return Inverse of Matrix, from Cache / Compute then Cache

cacheSolve <- function(x, ...) {
	mi <- x$getmi()						# Get Inverse Value from Cache
	if(!is.null(mi)) return(mi)				# <if cache available> Output [End]
	mi <- solve(x$getm(), ...)				# <if not> Compute Inverse of Input
	x$setmi(mi)							# Pass result to cache	
	mi								# Output (Inverse Value)
}
