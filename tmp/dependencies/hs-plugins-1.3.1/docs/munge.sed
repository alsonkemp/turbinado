
# de-boldify and <p>-ify the Contents.

/Contents/ { 
	:loop
	/Go to/ { 
		b end
	}
	s,<p>,,
        s,<b>,,
        s,</b>,,
        s,</p>,,
	n
	b loop
}
:end
