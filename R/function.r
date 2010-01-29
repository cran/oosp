FUNCTION = function (f, ..., self.reference=".")
{	objs = if (is.cleancall () ) collection (..., call=sys.call () [-2])
	else collection (..., resolve=FALSE)
	g = list ()
	g [[1]] = as.name ("{")
	g [[2]] = parse (text=paste (self.reference,
		"=environment(sys.function())", sep="") ) [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in itobj (b) ) g [[i + 2]] = b [[i]]
	}
	else g [[3]] = b
	body (f) = as.call (g)
	environment (f) = as.ENVIRONMENT (objs)
	structure (as.component (f), class="FUNCTION")
}

print.FUNCTION = function (f, ...)
{	g = format (args (f) )
	n = length (g)
	cat ("FUNCTION")
	cat (substring (g [1], 9), "\n")
	if (n > 2) cat (g [-c (1, n) ], sep="\n")
	cat ("attributes:\n")
	print (ls (environment (f) ) )
}

`$.FUNCTION` = function (x, name) get (name, environment (x) )
`$<-.FUNCTION` = function (x, name, value)
{	assign (name, value, environment (x) )
	x
}


