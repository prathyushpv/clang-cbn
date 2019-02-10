set(LLVM_LINK_COMPONENTS support)

add_clang_executable(clang-cbn
  ClangCBN.cpp
  )
target_link_libraries(clang-cbn
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )
