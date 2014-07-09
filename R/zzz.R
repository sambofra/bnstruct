THISPKG <- "bnstruct"
.bnstructPkgEnv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
	version <- packageDescription("bnstruct", fields="Version")
	packageStartupMessage("Welcome to bnstruct version ", version)
	#ldSetOptions()
##	bm <- ldStatus(TRUE)
##	snow <- ocParallelStatus()
# 
# 	setHook(packageEvent("ff", "attach"),
# 		function(...){
# 			#ldSetOptions(verbose=FALSE)
# 			#ldStatus(TRUE)
# 		})

	setHook(packageEvent("foreach", "attach"),
		function(...){
			ocParallelStatus()
		})
}
