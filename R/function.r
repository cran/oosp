#insufficient
#one line functions only
mutate.function = function (f, str, ...)
{	body (f) = parse (text=str)
	f
}

makepoly = function (k)
{	str = NULL
	n = length (k)
	if (n == 0) str = "0"
	else
	{	k = rev (k)
		str = as.character (k [1])
		for (i in iter (n, 2) )
		{	v = abs (k [i])
			s = if (k [i] < 0) "-" else "+"
			l = r = ""; if (i > 2) {l = "("; r = ")"}
			str = paste (l, str, r, "*x", s, v, sep="")
		}
	}
	mutate (function (x) NULL, str)
}

