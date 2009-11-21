is.object = function (obj) inherits (obj, "object")
as.object = function (obj) extend (obj, "object")

is.component = function (obj) inherits (obj, "component")
as.component = function (obj) extend (as.object (obj), "component")

extend = function (obj, cl)
{	if (inherits (obj, cl) ) obj
	else structure (obj, class=c (cl, class (obj) ) )
}

collection = function (..., resolve=TRUE, call=sys.call () )
{	objs = list (...)
	if (resolve)
	{	n = length (objs)
		names = names (objs)
		if (is.null (names) ) names = rep ("", n)
		args = match.call (call=call) [-1]
		for (i in iter (n) )
		{	if (names [i] == "" && is.name (args [[i]]) )
				names [i] = as.character (args [[i]])
		}
		names (objs) = names
	}
	as.collection (objs)
}

is.collection = function (obj) inherits (obj, "collection")
as.collection = function (obj) extend (as.component (obj), "collection")
"[.collection" = function (obj, ...) obj [[...]]
"[<-.collection" = function (obj, ..., value) {obj [[...]] = value; obj}

is.cleancall = function ()
{	k = sys.call (-1)
	clean = TRUE
	n = length (k)
	for (i in iter (n, 2) ) if (as.character (k [i]) == "...") clean = FALSE
	clean
}

