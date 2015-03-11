.onLoad = function(lib, pkg) 
{
   library.dynam("bnstruct", package = pkg, lib.loc = lib)
	if( R.version$arch == "x86_64" )
		MAX_NODES <- 64
	else
		MAX_NODES <- 32
  
	assign("bnstruct.log.indent.tracker", 0, envir = .GlobalEnv)
}#.ONLOAD

.onUnload = function(lib) 
{
  library.dynam.unload("bnstruct", libpath = lib)
}#.ONUNLOAD
