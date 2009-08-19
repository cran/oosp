compenv = function (..., hash=FALSE)
{	e = structure (extend (as.component (new.env (hash) ), "compenv"), hash=hash)
	objs = list (...)
	n = length (objs)
	if (n > 0)
	{	strs = names (objs)
		if (is.null (strs) ) stop ("compenv args must be named")
		else for (i in 1:n) assign (strs [i], objs [[i]], envir=e)
	}
	e
}

as.compenv = function (obj)
{	if (inherits (obj, "compenv") ) obj
	else if (inherits (obj, "environment") ) extend (as.component (obj), "compenv")
	else if (inherits (obj, "list") ) list.to.compenv (obj)
	else stop ("as.compenv not applicable")
}

is.compenv = function (e) inherits (e, "compenv")

#unhashed environments ****tend**** to be in reverse order
oospaslist.compenv = function (e, ...)
{	f = compenv.to.list (e)
	if (!is.hashed (e) ) f = rev (f)
	f
}
oospasdataframe.compenv = function (e, ...) as.data.frame (oospaslist.compenv (e) )

clone.compenv = function (e, ...)
	structure (as.compenv (compenv.clone (e, ...) ), hash=is.hashed (e) )

clone.environment = function (e, ...) compenv.clone (e, ...)

list.to.compenv = function (obj, hash=FALSE)
{	e = compenv (hash=hash)
	n = length (obj)
	if (n > 0)
	{	strs = names (obj)
		for (i in 1:n) assign (strs [i], obj [[i]], envir=e)
	}
	e
}

compenv.to.list = function (e, flags=pointer (list (e) ) )
{	obj = list ()
	n = length (e)
	if (n > 0)
	{	strs = ls (e)
		for (i in 1:n)
		{	x = get (strs [i], envir=e)
			if (inherits (x, "environment") )
			{	flagged = FALSE
				for (flag in flags () ) if (equals (x, flag) ) flagged = TRUE
				if (flagged) x = paste ("pruned", strs [i], sep=":")
				else
				{	flags [[length (flags) + 1]] = x
					x = compenv.to.list (x, flags)
				}
			}
			obj [[i]] = x
			names (obj) [i] = strs [i]
		}
	}
	obj
}

compenv.clone = function (e, flags=pointer (list () ) )
{	f = new.env ()
	flags [[length (flags) + 1]] = list (e, f)
	if (length (e) > 0)
	{	strs = ls (e)
		for (str in strs)
		{	x = get (str, envir=e)
			if (inherits (x, "environment") )
			{	flagged = NULL
				for (flag in flags () ) if (equals (x, flag [[1]]) ) flagged = flag [[2]]
				if (is.null (flagged) ) assign (str, clone (x, flags), envir=f)
				else assign (str, flagged, envir=f)
			}
			else assign (str, x, envir=f)
		}
	}
	f
}

equals.compenv = function (e1, e2, ...) (format (e1) == format (e2) )
oospprint.compenv = function (e, ...) print (oospaslist (e) )
is.hashed = function (e) attr (e, "hash")

