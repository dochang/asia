.PHONY: all clean

all: index.html asia.html

index.html: README.asciidoc
	asciidoc -b html4 -o $@ $?

asia.html: asia.asciidoc
	asciidoc -b html4 -o $@ $?

clean:
	rm -f index.html asia.html
