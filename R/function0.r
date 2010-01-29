mutate.function = function (f, str, ...)
{	b = NULL
	n = length (str)
	if (n == 1) b = parse (text=str)
	else if (n > 1)
	{	b = list ()
		b [[1]] = as.name ("{")
		for (i in 1:n) b [[i + 1]] = parse (text=str [i]) [[1]]
		b = as.call (b)
	}
	body (f) = b
	f
}

