#duplication
#todo: fix
.mclass.method = function (f)
{	g = list ()
	g [[1]] = as.name ("{")
	g [[2]] = parse (text=".=environment(sys.function())$context") [[1]]
	g [[3]] = parse (text="..=environment(sys.function())$attributes") [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in itobj (b) ) g [[i + 3]] = b [[i]]
	}
	else g [[4]] = b
	body (f) = as.call (g)
	structure (f, class="Method")
}

.mclass.constructor = function (f)
{	g = list ()
	g [[1]] = as.name ("{")
	g [[2]] = parse (text="super=.mobject.super") [[1]]
	g [[3]] = parse (text=".=environment(sys.function())$context") [[1]]
	g [[4]] = parse (text="..=environment(sys.function())$attributes") [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in itobj (b) ) g [[i + 4]] = b [[i]]
	}
	else g [[5]] = b
	body (f) = as.call (g)
	structure (f, class="Method")
}

.mobject.super = function (...)
{	cobj = environment (sys.function (-1) )
	cobj$super$context = cobj$context
	cobj$super$constructor (...)
}

.mclass.getmethod = function (cobj, mid)
{	i = which (mid == cobj$mid)
	while (length (i) == 0)
	{	super = cobj$super
		if (is.null (super) ) stop (paste ("method", mid, "not found") )
		cobj = super
		i = which (mid == cobj$mid)
	}
	get (mid, envir=cobj$methods)
}

#this function is intended to be called by the invocation operator only
.mobject.invoke = function (obj, mid, invocation)
{	invocation [[1]] = as.name (".oosp.method")
	.oosp.method <<- .mclass.getmethod (obj$.class, mid)
	obj$.class$context = obj
	eval (invocation, sys.frame (-3) )
}

#invocation.on = function () assign ("~", .invocation.operator, envir=.GlobalEnv)
#invocation.off = function () assign ("~", base::`~`, envir=.GlobalEnv)

#UseMethod fails
#need the double-up, not sure why
"~" = function (obj, invocation)
{	if (missing (invocation) ) `~.default` (obj, invocation)
	else if (is.MObject (obj) )
	{	invocation = deparse (substitute (invocation) )
		`~.MObject` (obj, invocation)
	}
	else `~.default` (obj, invocation)
}

"~.MObject" = function (obj, invocation)
{	obj = eval (obj)
	invocation = as.call (parse (text=invocation) [[1]])
	mid = as.character (invocation [[1]])
	.mobject.invoke (obj, mid, invocation)
}

"~.default" = function (obj, ...)
{	call = sys.call (-1)
	call [[1]] = as.name ("~")
	e = sys.frame (-2)
	f = new.env ()
	environment (f) = e
	assign ("~", base::`~`, envir=f)
	obj = eval (call, envir=f)
	environment (obj) = e
	obj
}

freemethod = function (f, ...)
{	objs = if (is.cleancall () ) collection (..., call=sys.call () [-2])
	else collection (..., resolve=FALSE)
	g = list ()
	g [[1]] = as.name ("{")
	g [[2]] = parse (text=".=environment(sys.function())") [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in itobj (b) ) g [[i + 2]] = b [[i]]
	}
	else g [[3]] = b
	body (f) = as.call (g)
	environment (f) = as.compenv (objs)
	structure (as.component (f), class="freemethod")
}

hypermethod = function (obj, mid)
{	m = .mclass.getmethod (obj$.class, mid)
	f = function (...)
	{	.$obj$.class$context = .$obj
		.$m (...)
	}
	extend (freemethod (f, obj, m), "hypermethod")
}

print.freemethod = function (f, ...)
{	environment (f) = .GlobalEnv
	attributes (f) = NULL
	print.default (f)
}


