#
##  Makefile for Ray Tracer

MACHINE = $(shell uname -s)

LIBS = -lpthread

CFLAGS = -O2 -g -std=c++11

CC = g++

INCLUDE =-Isrc/gml/src -Isrc/objects -Isrc/materials -Isrc/

FORCE_INCLUDE=-include rtstd.h

.SUFFIXES: .o .cpp .cxx

.cpp.o: 
		$(CC) $(CFLAGS) $(INCLUDE) $(FORCE_INCLUDE) -c -o $*.o $<

.cxx.o: 
		$(CC) $(CFLAGS) $(INCLUDE) -c -o $*.o $<

ALL.O = src/bmp.o src/main.o src/tracer.o\
		src/materials/lambertian.o src/materials/metallic.o src/materials/dielectric.o\
		src/gml/src/matrix.o src/gml/src/collision.o

ALL.H = $(wildcard src/*.h)

ray: $(ALL.O) $(ALL.H)
		$(CC) $(CFLAGS) -o $@ $(ALL.O) $(INCLUDE) $(LIBDIR) $(LIBS)

clean:
		rm -f $(ALL.O)
