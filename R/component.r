as.component = function (obj)
	if (is.component (obj) ) obj else extend (as.object (obj), "component")

is.component = function (obj) inherits (obj, "component")

