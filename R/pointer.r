pointer = function (obj, hash=FALSE)
{	f = function (x) NULL
	f = mutate (f, "attributes(sys.function())[[2]]$obj")
	e = compenv (obj=obj, hash=hash)
	structure (f, class=c ("pointer", "object", "function"), e=e)
}

map.pointer = function (ptr, obj, ...)
{	e = ptrenv (ptr)
	e$obj = obj
}

oospprint.pointer = function (ptr, ...) cat ("pointer:", class (ptr () ) [[1]], "\n")

ptrenv = function (ptr) attributes (ptr) [[2]]

#implement an oosp generic, then use ptr?
length.pointer = function (x) length (ptrenv (x)$obj)

"[.pointer" = function (ptr, i, ...)
{	e = ptrenv (ptr)
	e$obj [i]
}

#maybe get the standard function to do the work instead?
"[<-.pointer" = function (ptr, i, ..., value)
{	e = ptrenv (ptr)
	e$obj [i] = value
	ptr
}

"[[.pointer" = function (ptr, i)
{	e = ptrenv (ptr)
	e$obj [[i]]
}

"[[<-.pointer" = function (ptr, i, ..., value)
{	e = ptrenv (ptr)
	e$obj [[i]] = value
	ptr
}




