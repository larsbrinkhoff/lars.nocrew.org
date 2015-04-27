CPP = gcc -E -
PERL = perl

SITE = $(shell /bin/sh site)

all: index.html

index.html: index.html.in Makefile
	$(PERL) hpp.perl $(SITE) < index.html.in > index.html

#	$(CPP) -D$(SITE) < index.html.in | grep -v '^#' > index.html
