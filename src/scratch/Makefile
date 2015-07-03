CC=g++ -std=c++14
CFLAGS=-c -Wall
LDFLAGS=
SOURCES=scratch.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXE=scratch

all: $(SOURCES) $(EXE)

$(EXE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) $(LIB_INC) $(LIB) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $(INC) $(LIB_INC) $(LIB) $< -o $@

clean:
	rm -f $(EXE) $(OBJECTS)
