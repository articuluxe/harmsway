CXXFLAGS=$(CXX_FLAGS) -Wno-unused-parameter -c -g
LDFLAGS=
SOURCES=scratch.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXE=scratch
INC=
LIB=
LIB_INC=

all: $(SOURCES) $(EXE)

$(EXE): $(OBJECTS)
	$(CXX) $(LDFLAGS) $(OBJECTS) $(LIB_INC) $(LIB) -o $@

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INC) $(LIB_INC) $(LIB) $< -o $@

clean:
	rm -f $(EXE) $(OBJECTS)
