# clang-cbn
A source to source transformation tool for C using clang LibTooling, which creates a new c source file which emulates call-by-name calling convension.

## How to compile and run
1. Clone the llvm repository using 'git clone https://github.com/llvm/llvm-project.git'
2. Extract the zip file to llvm/clang/tools/clang-cbn. (The files including ClangCBN.cpp should be inside llvm/clang/tools/clang-cbn)
3. Add the line 'add_subdirectory(clang-cbn)' to llvm/clang/tools/CMakeLists.txt
4. Compile the clang source tree using cmake.
5. A new executable named clang-cbn will be created in the bin directory under the build directory.
5. Invoke the tool using bin/clang-cbn inputfile_path -- (Don't forget the -- in the end)
6. The tool will print the rewtitten code to stdout. Redirect it to a file and pass it to the c compiler.

## Files in the directory
1. ClangCBN.cpp: This file contains the code for source to source transformation of a c program to emulates call-by-name calling convension.
2. CMakeLists.txt CMake file to build ClangCBN.cpp
