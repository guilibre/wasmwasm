add_executable(ww_test_runner runner.cpp)

target_link_libraries(ww_test_runner PRIVATE ${PROJECT_NAME}_lib binaryen jsoncpp_static compiler_flags)
target_include_directories(ww_test_runner PRIVATE ${binaryen_SOURCE_DIR}/src ../src)

target_link_options(ww_test_runner PRIVATE
    --preload-file ${CMAKE_BINARY_DIR}/math/math.wasm@/math.wasm
    -sSTACK_SIZE=5242880
)
add_dependencies(ww_test_runner math)

find_program(NODE_EXECUTABLE node REQUIRED)

file(GLOB WW_TEST_CASES CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cases/*.ww)
foreach(case_path ${WW_TEST_CASES})
    get_filename_component(case_name ${case_path} NAME_WE)
    set(expect_path ${CMAKE_CURRENT_SOURCE_DIR}/cases/${case_name}.expect)
    add_test(
        NAME ww_test_${case_name}
        COMMAND ${NODE_EXECUTABLE}
                ${CMAKE_CURRENT_SOURCE_DIR}/run_case.js
                ${CMAKE_CURRENT_BINARY_DIR}/ww_test_runner.js
                ${case_path} ${expect_path}
    )
endforeach()

file(GLOB WW_PATCH_TEST_CASES CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cases/*.json)
foreach(case_path ${WW_PATCH_TEST_CASES})
    get_filename_component(case_name ${case_path} NAME_WE)
    set(expect_path ${CMAKE_CURRENT_SOURCE_DIR}/cases/${case_name}.expect)
    add_test(
        NAME ww_test_${case_name}
        COMMAND ${NODE_EXECUTABLE}
                ${CMAKE_CURRENT_SOURCE_DIR}/run_case.js
                ${CMAKE_CURRENT_BINARY_DIR}/ww_test_runner.js
                ${case_path} ${expect_path} --patch
    )
endforeach()
