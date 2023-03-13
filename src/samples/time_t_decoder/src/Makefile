CXXFLAGS=-std=c++17 $(CXX_FLAGS) -Wno-unused-parameter -c -g
LDFLAGS=-g
SOURCES=time_t_decoder.cpp
OBJECTS=$(SOURCES:.cpp=.o)
DEPS=$(OBJECTS:.o=.d)
EXE=time_t_decoder
INC=
LIB_INC=
LIB=

all: $(SOURCES) $(EXE)

$(EXE): $(OBJECTS)
	$(CXX) $(LDFLAGS) $(OBJECTS) $(LIB_INC) -o $@ $(LIB)

.cpp.o:
	$(CXX) -MMD -MP $(CXXFLAGS) $(INC) $(LIB_INC) $(LIB) $< -o $@

clean:
	rm -f $(EXE) $(OBJECTS)

-include $(DEPS)
