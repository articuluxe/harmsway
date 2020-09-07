CXXFLAGS=-std=c++17 $(CXX_FLAGS) -Wno-unused-parameter -c -g
LDFLAGS=-g
SOURCES=scratch.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXE=scratch
INC=
LIB_INC=
LIB=

all: $(SOURCES) $(EXE)

$(EXE): $(OBJECTS)
	$(CXX) $(LDFLAGS) $(OBJECTS) $(LIB_INC) -o $@ $(LIB)

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INC) $(LIB_INC) $(LIB) $< -o $@

clean:
	rm -f $(EXE) $(OBJECTS)
