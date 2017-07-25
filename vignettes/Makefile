DOT=$(wildcard *.dot)
PNG=$(DOT:.dot=.png)
SVG=$(DOT:.dot=.svg)

all: $(SVG) $(PNG)

%.svg: %.dot
	dot $< -Tsvg >$@

%.png: %.dot
	dot $< -Tpng >$@

clean:
	$(RM) *.svg *.png

.PHONY: all clean
