CHICKENPREFIX=/usr
CHICKEN=$(CHICKENPREFIX)/bin/chicken
CHICKENINCLUDE=$(CHICKENPREFIX)/include/chicken
CHICKENLIB=$(CHICKENPREFIX)/lib

CC=clang
CFLAGS=-Wall -Wno-unused -O3 -g -I../../include -fsanitize-coverage=indirect-calls,edge -fsanitize=address -I$(CHICKENINCLUDE)
CHICKENFLAGS=-optimize-level 3

all: module.a
ops.o: ops.c
	$(CC) $(CFLAGS) -DC_EMBEDDED ops.c -c -o ops.o
ops.c: ops.scm
	$(CHICKEN) ops.scm -output-file ops.c $(CHICKENFLAGS)
module.a: module.o ops.o
	bash ./link.sh module.a $(CHICKENLIB)/libchicken.a ops.o
	ranlib module.a
module.o: module.c
	$(CC) $(CFLAGS) -fPIC -c module.c -o module.o
clean:
	rm -rf module.o module.a ops.o ops.c
