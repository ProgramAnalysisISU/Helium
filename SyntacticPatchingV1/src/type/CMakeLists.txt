add_library(HeliumType
  IOHelper.cpp
  Type.cpp
  
  Cache.cpp
  Snippet.cpp
  SnippetManager.cpp
  SnippetAction.cpp)
target_link_libraries(HeliumType HeliumUtils)
