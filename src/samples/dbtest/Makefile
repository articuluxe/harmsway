CXXFLAGS=$(CXX_FLAGS) -Wno-unused-parameter -c -g
LDFLAGS=-g
SOURCES=db.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXE=db
FREETDS_ROOT=/usr/local/freetds/0.95.95
INC=-I $(FREETDS_ROOT)/include
LIB_INC=-L $(FREETDS_ROOT)/lib64
LIB=-lsybdb

all: $(SOURCES) $(EXE)

$(EXE): $(OBJECTS)
	$(CXX) $(LDFLAGS) $(OBJECTS) $(LIB_INC) -o $@ $(LIB)

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INC) $(LIB_INC) $(LIB) $< -o $@

clean:
	rm -f $(EXE) $(OBJECTS)
