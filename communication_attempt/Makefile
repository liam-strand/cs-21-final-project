# Makefile for locality (Comp 40 Assignment 3)
#
# Includes build rules for a2test and ppmtrans.
#
# This Makefile is more verbose than necessary.  In each assignment
# we will simplify the Makefile using more powerful syntax and implicit rules.
#
# Last updated: February 16, 2016

# Authors: Liam Strand && Roger Burtonpatel
# Date: 2.24.2022
# Assignment: Arith

############## Variables ###############

CC = gcc # The compiler being used

# Updating include path to use Comp 40 .h files and CII interfaces
IFLAGS = -I/opt/homebrew/lib/erlang/lib/erl_interface-5.2.2/include

CFLAGS =  $(IFLAGS)

LDFLAGS = -g3 -L/opt/homebrew/lib/erlang/lib/erl_interface-5.2.2/lib

LDLIBS = -lei

# Collect all .h files in your directory.
# This way, you can never forget to add
# a local .h file in your dependencies.
#
# This bugs Mark, who dislikes false dependencies, but
# he agrees with Noah that you'll probably spend hours
# debugging if you forget to put .h files in your
# dependency list.
INCLUDES = $(shell echo *.h)

############### Rules ###############
	
## Making object files
%.o: %.c $(INCLUDES)
	$(CC) $(CFLAGS) -fPIC -c $< -o $@


## Making shared object libraries

extprg: complex.o ei.o erl_comm.o
	gcc $(LDFLAGS) $^ -o $@ $(LDLIBS)

erlcoms.so: erlcoms.c $(INCLUDES)
	$(CC) $(LDFLAGS) $(CFLAGS) -fPIC -shared -o $@ $^ $(LDLIBS)

erlcoms: erlcoms.c $(INCLUDES)
	$(CC) $(LDFLAGS) $(CFLAGS) -o $@ $^ $(LDLIBS)

## Linking step (.o -> executable program)

clean:
	rm -rf extprg *.dSYM *.beam *.o
