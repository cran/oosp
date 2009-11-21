mclass = function (super, ..., hybrid=TRUE, abstract=FALSE)
{	cc = sys.call (-1)
	cid = as.character (cc [[1]])
	generate = TRUE
	if (cid == ".mclass.init")
	{	cid = as.character (cc [[2]])
		generate = FALSE
	}
	if (missing (super) ) super = "MObject"
	else
	{	sid = as.character (sys.call () [[2]])
		if (!.mclass.exists (sid) )
		{	.mclass.init = function (x) NULL
			body (.mclass.init) = body (super)
			eval (as.call (parse (text=paste (".mclass.init(", sid, ")") ) [[1]]) )
		}
		super = sid
	}
	cs = cobj = NULL
	if (.mclass.exists (cid) ) cobj = .mclass.get (cid)
	else
	{	cd = sys.function (-1)
		cs = .mclass.parse (cid, cd)
		cobj = .mclass.generate (cid, super, abstract, cs$constructor, cs$mid, cs$methods)
	}
	if (generate)
	{	if (cobj$abstract) stop (paste ("class", cid, "is abstract") )
		oobj = .mobject.generate (cid, hybrid)
		if (ifst (cobj$constructor) )
		{	mc = match.call (cobj$constructor, cc)
			mc [[1]] = as.name (".oosp.constructor")
			.oosp.constructor <<- cobj$constructor
			cobj$context = oobj
			eval (mc, sys.frame (-2) )
		}
		else
		{	mc = match.call (sys.function (-1), cc)[-1]
			ss = names (mc)
			for (i in itobj (mc) )
				if (ss [i] != "") assign (ss [i], mc [[i]], envir=oobj)
		}
		oobj
	}
}

.mclass.parse = function (cid, cd)
{	constructor = NULL
	mid = character ()
	methods = list ()
	b = as.list (body (cd) )
	for (expr in b)
	{	if (class (expr) == "=")
		{	right = expr [[3]]
			if (class (right) == "call")
			{	id = as.character (expr [[2]])
				f = eval (right)
				if (cid == id) constructor = f
				else
				{	mid = c (mid, id)
					methods [[length (methods) + 1]] = f
				} 
			}
		}
	}
	collection (constructor, mid, methods)
}

.mclass.generate = function (cid, super=NULL, abstract=FALSE,
	constructor=NULL, mid=character (), methods=list () )
{	if (ifst (super) ) super = .mclass.get (super)
	context = NA
	attributes = compenv ()
	methodenv = compenv ()	
	cobj = extend (compenv (cid, super, abstract, context, attributes,
		mid, methods=methodenv), "mclass")
	if (ifst (constructor) )
	{	constructor = .mclass.constructor (constructor)
		environment (constructor) = cobj
	}
	cobj$constructor = constructor
	for (i in itobj (mid) )
	{	f = .mclass.method (methods [[i]])
		environment (f) = cobj
		assign (mid [i], f, envir=methodenv) 
	}
	.mclass.add (cobj)
	invisible (cobj)
}

.mclass.exists = function (cid) any (cid == .oosp.cid)
.mclass.get = function (cid) .oosp.image [[which (cid ==.oosp.cid)]]
.mclass.add = function (cobj)
{	n = length (.oosp.cid) + 1
	.oosp.cid [n] <<- cobj$cid
	.oosp.image [[n]] <<- cobj
}

mdelete = function (cid)
{	i = (cid == .oosp.cid)
	.oosp.cid <<- .oosp.cid [!i]
	.oosp.image <<- .oosp.image [!i]
}

.mobject.generate = function (cid, hybrid=TRUE)
{	cobj = .mclass.get (cid)
	oobj = compenv (.class=cobj)
	if (hybrid)
	{	classes = cid
		while (ifst (cobj$super) )
		{	cobj = cobj$super
			classes = c (classes, cobj$cid)
		}
		class (oobj) = classes
	}
	else class (oobj) = "MObject"
	oobj
}

is.MObject = function (obj) inherits (obj, "MObject")

print.MObject = function (obj, ...) cat ("MObject:", obj$.class$cid, "\n")

