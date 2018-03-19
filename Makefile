# compiler to use
CC = gcc

# flags to pass compiler
CFLAGS = -O2 -Wall

# name for executable
EXE = vm

# space-separated list of header files
HDRS =

# space-separated list of libraries, if any,
# each of which should be prefixed with -l
LIBS =

# space-separated list of source files
SRCS = vm.c

# automatically generated list of object files
OBJS = $(SRCS:.c=.o)


# default target
$(EXE): $(OBJS) $(HDRS) Makefile
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

# dependencies
$(OBJS): $(HDRS) Makefile

# housekeeping
.PHONY: clean
clean:
	rm -f core $(EXE) *.o
