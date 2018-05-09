add_subdirectory(utils)
add_subdirectory(type)
add_subdirectory(parser)

add_executable(helium main.cc)
target_link_libraries(helium
        HeliumUtils
        HeliumType
        HeliumParser)
target_link_libraries(helium
        ${Boost_FILESYSTEM_LIBRARY}
        ${Boost_SYSTEM_LIBRARY}
        ${Boost_LIBRARIES}
        ${GTEST_BOTH_LIBRARIES})

target_link_libraries(helium pugixml sqlite3)
