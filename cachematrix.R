###########################################################
#
# makeCacheMatrix & cacheSolve
# Coursera rprog-004 homework assignment #3
# Dexter Taylor (github ID: binarymachines)
# 
# makeCacheMatrix creates an R matrix "object" capable
# of caching its own inverse. It exposes functions
# for setting/getting its matrix data, setting/getting the
# cached matrix-inverse data, and clearing the cache.
#
#
# cacheSolve takes a single input parameter:
# the special matrix object returned by makeCacheMatrix.
# It will either return the cached inverse matrix or, 
# in the case of a "cache miss", calculate the inverse,
# store it, and return the solution.
#
#
############################################################


#
#  makeCacheMatrix will create a new object, with the matrix to be stored
#  passed as a parameter.
#
makeCacheMatrix <- function(inputMtx = matrix()){
	mtx <- inputMtx
	cache <- NULL
	
	# when the matrix changes,
	# clear the cache automatically
	#
	set <- function(inputMx) {
		mtx <<- inputMx
		clearCache()
	}
	
	# return the unaltered matrix
	#
	get <- function() {
		return(mtx)
	}
	
	# store the calcuated matrix value
	#
	cacheMatrix <- function(inverseMtx){
		cache <<- inverseMtx
	}
	
	# empty the cache
	#
	clearCache <- function(){		
		cache <<- NULL
	}
	
	# return the cached matrix inverse 
	# (or null if cache is empty)
	#
	readCache <- function(){
		return(cache)
	}

	list(set=set, get=get, cacheMatrix=cacheMatrix, clearCache=clearCache, readCache=readCache)	
}


#
# cacheSolve will return the inverse of a matrix.
# the input parameter must be the the result of a call to makeCacheMatrix()
#
cacheSolve <- function(cacheMatrix, ...){
			
	# if the inverse has already been calcuated and stored...
	storedInverse <- cacheMatrix$readCache()
	if(!is.null(storedInverse)){
		# return the cached value
		message('returning cached matrix inverse...')
		return(storedInverse)		
	}	
	else{
		# otherwise calculate the inverse
		message('calculating matrix inverse...')		   
		mxInv <- solve(cacheMatrix$get())
		# and cache the solution
		cacheMatrix$cacheMatrix(mxInv)
		return(mxInv)
	}
}
