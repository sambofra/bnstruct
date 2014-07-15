# BootstrapIterator <- function(dataset, ...)
# {
#   object <- new("BootstrapIterator", dataset = dataset, ...)
#   object
# }
# 
# # validator
# setValidity("BootstrapIterator",
#             function(object)
#             {
#               retval <- NULL
#               if (object@dataset == NULL)
#               {
#                 retval <- c(retval, "dataset not valid")
#               }
#               
#               if (is.null(retval)) return (TRUE)
#               return (retval)
#             }
# )
# 
# 
# setMethod("getNext",
#           "BootstrapIterator",
#           function(object)
#           {
#             object@current <- object@current + 1
#             object@
#           })