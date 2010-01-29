ENVIRONMENT = function (..., hash=FALSE)
{	objs = if (is.cleancall () ) collection (..., call=sys.call () )
	else collection (..., resolve=FALSE)
	if (ifst (objs) ) list.to.ENVIRONMENT (objs, hash)
	else structure (extend (as.component (new.env (hash) ), "ENVIRONMENT"), hash=hash)
}

is.ENVIRONMENT = function (e) inherits (e, "ENVIRONMENT")

as.ENVIRONMENT = function (obj)
{	if (is.ENVIRONMENT (obj) ) obj
	else if (inherits (obj, "environment") ) extend (as.component (obj), "ENVIRONMENT")
	else if (inherits (obj, "list") ) list.to.ENVIRONMENT (obj)
	else stop ("as.ENVIRONMENT not applicable")
}

"==.ENVIRONMENT" = function (e1, e2) (format (e1) == format (e2) )
is.hashed = function (e) attr (e, "hash")

print.ENVIRONMENT = function (e, ...)
{	obj = as.list (e)
	if (if0 (obj) ) cat ("empty ENVIRONMENT\n")
	else for (i in itobj (obj) )
	{	cat ("$", names (obj) [i], "\n", sep="")
		if (is.ENVIRONMENT (obj [[i]]) ) cat (format (obj [[i]]), "\n" )
		else print (obj [[i]])
	}
}

list.to.ENVIRONMENT = function (obj, hash=FALSE)
{	names = names (obj)
	if (is.null (names) || any (names == "") )
		stop ("ENVIRONMENT args must be named (or nameable)")
	e = ENVIRONMENT (hash=hash)
	for (i in itobj (obj) ) assign (names [i], obj [[i]], envir=e)
	e
}

clone.ENVIRONMENT = function (e, ...)
	structure (as.ENVIRONMENT (ENVIRONMENT.clone (e, ...) ), hash=is.hashed (e) )

clone.environment = function (e, ...) ENVIRONMENT.clone (e, ...)

ENVIRONMENT.clone = function (e, flags=pointer (list () ) )
{	f = new.env ()
	flags [[length (flags) + 1]] = list (e, f)
	if (length (e) > 0)
	{	strs = ls (e)
		for (str in strs)
		{	x = get (str, envir=e)
			if (inherits (x, "environment") )
			{	flagged = NULL
				for (flag in flags () ) if (`==.ENVIRONMENT` (x, flag [[1]]) ) flagged = flag [[2]]
				if (is.null (flagged) ) assign (str, clone (x, flags), envir=f)
				else assign (str, flagged, envir=f)
			}
			else assign (str, x, envir=f)
		}
	}
	f
}



