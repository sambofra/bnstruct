.onLoad = function(lib, pkg) 
{
   library.dynam("bnstruct", package = pkg, lib.loc = lib)
	if( R.version$arch == "x86_64" )
		MAX_NODES <- 64
	else
		MAX_NODES <- 32
}#.ONLOAD

.onUnload = function(lib) 
{
  library.dynam.unload("bnstruct", libpath = lib)
}#.ONUNLOAD