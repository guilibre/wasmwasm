add_executable(${PROJECT_NAME}
    main.cpp
    db.cpp
    auth.cpp
    routes_users.cpp
    routes_patches.cpp
)

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
)

target_include_directories(${PROJECT_NAME}
    PRIVATE ${CMAKE_SOURCE_DIR}/vendor/instantclient/sdk/include
)

target_link_libraries(${PROJECT_NAME}
    PRIVATE compiler_flags ${OCCI} ${CLNTSH} Crow sodium jwt-cpp::jwt-cpp OpenSSL::Crypto
)

set_target_properties(${PROJECT_NAME} PROPERTIES
    BUILD_RPATH ${CMAKE_SOURCE_DIR}/vendor/instantclient
    INSTALL_RPATH ${CMAKE_SOURCE_DIR}/vendor/instantclient
)
target_link_options(${PROJECT_NAME} PRIVATE -Wl,--disable-new-dtags)

target_compile_options(${PROJECT_NAME} PRIVATE -include cstdint)
