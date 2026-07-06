add_executable(score_test_runner runner.cpp)

target_link_libraries(score_test_runner PRIVATE ${PROJECT_NAME}_lib compiler_flags)
target_include_directories(score_test_runner PRIVATE ../src)

target_link_options(score_test_runner PRIVATE
    -sSTACK_SIZE=5242880
)

find_program(NODE_EXECUTABLE node REQUIRED)

file(GLOB SCORE_TEST_CASES CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cases/*.score)
foreach(case_path ${SCORE_TEST_CASES})
    get_filename_component(case_name ${case_path} NAME_WE)
    set(expect_path ${CMAKE_CURRENT_SOURCE_DIR}/cases/${case_name}.expect)
    add_test(
        NAME score_test_${case_name}
        COMMAND ${NODE_EXECUTABLE}
                ${CMAKE_CURRENT_SOURCE_DIR}/run_case.js
                ${CMAKE_CURRENT_BINARY_DIR}/score_test_runner.js
                ${case_path} ${expect_path}
    )
endforeach()
