# run: cmake -P cmake-generate.cmake
# cmake --help-command-list >> AllVariables.txt

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/AllVariables.txt "")
GET_CMAKE_PROPERTY(res VARIABLES)
FOREACH(var ${res})
FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/AllVariables.txt
"${var} \"${${var}}\"\n")
ENDFOREACH(var ${res})
