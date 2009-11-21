pointer = function (obj)
{	f = function () pointer.image ()
	extend (freemethod (f, obj), "pointer")
}

pointer.image = function () environment (sys.function (-1) )$obj

length.pointer = function (x) length (environment (x)$obj)
map.pointer = function (ptr, obj, ...) environment (ptr)$obj = obj
print.pointer = function (ptr, ...) cat ("pointer:", class (ptr () ) [[1]], "\n", sep="")

"[.pointer" = function (ptr, ...) environment (ptr)$obj [...]
"[[.pointer" = function (ptr, ...) environment (ptr)$obj [[...]]
"[<-.pointer" = function (ptr, ..., value)
{	e = environment (ptr)
	e$obj [...] = value
	ptr
}
"[[<-.pointer" = function (ptr, ..., value)
{	e = environment (ptr)
	e$obj [[...]] = value
	ptr
}



