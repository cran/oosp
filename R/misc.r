ifst = function (obj) (!is.null (obj) && length (obj) > 0)
if0 = function (obj) (!is.null (obj) && length (obj) == 0)
if1 = function (obj) (ifst (obj) && length (obj) == 1)
iter = function (n, min=1) if (n < min) numeric () else min:n
itobj = function (obj) if (ifst (obj) ) 1:length (obj) else numeric ()

#uncompressed only
datafile = function (pkg, file, global=FALSE, preview=FALSE, ...)
{	path = if (missing (pkg) ) "" else paste (.find.package (pkg), "/data/", sep="")
	path = paste (path, file, sep="")
	if (!file.exists (path) ) path = paste (path, ".csv", sep="")
	if (!file.exists (path) ) stop ("file does not exist")
	d = read.csv (path, ...)
	if (global) for (i in itobj (d) ) assign (names (d) [i], d [[i]], envir=.GlobalEnv)
	if (preview) preview (d)
	if (global || preview) invisible (d) else d
}

preview = function (d)
{	n = nrow (d)
	s = length (d)
	ifactor = inumeric  = iother = numeric ()
	for (i in 1:s)
	{	if (inherits (d [[i]], "factor") ) ifactor = c (ifactor, i)
		else if (inherits (d [[i]], "integer") || inherits (d [[i]], "numeric") )
			inumeric = c (inumeric, i)
		else iother = c (iother, i)
	}
	if (ifst (ifactor) )
	{	cat ("factors:  ")
		for (i in ifactor)
		{	cat (names (d) [i], " in {", sep="")
			levs = levels (d [[i]])
			levs = if (length (levs) > 6)
				paste (paste (levs [1:6], collapse=", "), ", ...", sep="")
			else paste (levs, collapse=", ")
			cat (levs, "}\n", sep="")
			if (i != ifactor [length (ifactor)]) cat ("          ")
		}
	}
	if (ifst (inumeric) )
	{	cat ("numerics: ")
		for (i in inumeric)
		{	cat (names (d) [i], " in (", sep="")
			x = d [[i]]
			cat (min (x, na.rm=TRUE), ", ", max (x, na.rm=TRUE), ")\n", sep="")
			if (i != inumeric [length (inumeric)]) cat ("          ")
		}
	}
	if (ifst (iother) )
	{	cat ("others:   ")
		cat (paste (names (d) [iother], collapse=", "), "\n")
	}
	v = numeric ()
	valid = rep (TRUE, n)
	for (i in 1:s)
	{	validi = is.finite (d [[i]])
		v [i] = sum (validi)
		valid = valid & validi
	}
	nv = sum (valid)
	flag = if (n == nv) "<clean>" else "<defective>"
	cat ("\ndata.frame ", flag, " ~ *(", n, ", ", s, ")\n", sep="")
	if (n < 11) print (d)
	else print (d [c (1:3, (n - 2):n),])
	
	if (n > nv)
	{	v = data.frame (matrix (v, nc=s), nv, row.names="nv")
		names (v) = c (names (d), "overall")
		cat ("\n")
		print (v)
	}

}

as.r.default = function (x, ...)
{	if (is.null (x) ) cat ("NULL\n")
	else if (if0 (x) ) print (x)
	else
	{	if (inherits (x, "character") )
		{	i = !is.na (x)
			x [i] = paste ("\"", x [i], "\"", sep="")
		}
		y = if (if1 (x) ) x
		else	paste ("c(", paste (x, collapse=","), ")", sep="")
		cat (y, "\n")
	}
}

trim = function (x)
{	for (i in itobj (x) ) x [i] = trim.scalar (x [i])
	x
}

trim.scalar = function (x)
{	if (ifst (x) && is.character (x) )
	{	n = nchar (x)
		if (n == 0) ""
		else
		{	y = strsplit (x, "") [[1]]
			i = which (y != " ")
			if (if0 (i) ) ""
			else
			{	imin = min (i)
				imax = max (i)
				substr (x, imin, imax)
			}
		}
	}
	else ""
}

