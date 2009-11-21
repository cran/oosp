compenv = function (..., hash=FALSE)
{	objs = if (is.cleancall () ) collection (..., call=sys.call () )
	else collection (..., resolve=FALSE)
	if (ifst (objs) ) list.to.compenv (objs, hash)
	else structure (extend (as.component (new.env (hash) ), "compenv"), hash=hash)
}

is.compenv = function (e) inherits (e, "compenv")

as.compenv = function (obj)
{	if (is.compenv (obj) ) obj
	else if (inherits (obj, "environment") ) extend (as.component (obj), "compenv")
	else if (inherits (obj, "list") ) list.to.compenv (obj)
	else stop ("as.compenv not applicable")
}

"==.compenv" = function (e1, e2) (format (e1) == format (e2) )
is.hashed = function (e) attr (e, "hash")

print.compenv = function (e, ...)
{	obj = as.list (e)
	if (if0 (obj) ) cat ("empty compenv\n")
	else for (i in itobj (obj) )
	{	cat ("$", names (obj) [i], "\n", sep="")
		if (is.compenv (obj [[i]]) ) cat (format (obj [[i]]), "\n" )
		else print (obj [[i]])
	}
}

list.to.compenv = function (obj, hash=FALSE)
{	names = names (obj)
	if (is.null (names) || any (names == "") )
		stop ("compenv args must be named (or nameable)")
	e = compenv (hash=hash)
	for (i in itobj (obj) ) assign (names [i], obj [[i]], envir=e)
	e
}

clone.compenv = function (e, ...)
	structure (as.compenv (compenv.clone (e, ...) ), hash=is.hashed (e) )

clone.environment = function (e, ...) compenv.clone (e, ...)

compenv.clone = function (e, flags=pointer (list () ) )
{	f = new.env ()
	flags [[length (flags) + 1]] = list (e, f)
	if (length (e) > 0)
	{	strs = ls (e)
		for (str in strs)
		{	x = get (str, envir=e)
			if (inherits (x, "environment") )
			{	flagged = NULL
				for (flag in flags () ) if (`==.compenv` (x, flag [[1]]) ) flagged = flag [[2]]
				if (is.null (flagged) ) assign (str, clone (x, flags), envir=f)
				else assign (str, flagged, envir=f)
			}
			else assign (str, x, envir=f)
		}
	}
	f
}

