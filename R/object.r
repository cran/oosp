as.object = function (obj) if (is.object (obj) ) obj else extend (obj, "object")
is.object = function (obj) inherits (obj, "object")

extend = function (obj, cl) structure (obj, class=c (cl, class (obj) ) )

