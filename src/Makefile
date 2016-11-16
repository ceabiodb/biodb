all:
	# Settings for macOS. TODO should output .dylib instead of .so, and modify loading in R code.
	gcc -I/usr/local/include -shared -L/usr/local/lib -o closeMatchPpm.so closeMatchPpm.c -lR

clean:
	$(RM) *.so

.PHONY: all clean
