.hasField <- function (obj,name) length(grep(as.name(name),names(as.list(obj)))) > 0
.checkNames <- function (obj,requiredNames) for(name in requiredNames) if(!.hasField(obj,name)) stop('missing column for ', name)

.setDefault <- function (obj,name,default) if (.hasField(obj,name)) obj[[name]] else default
