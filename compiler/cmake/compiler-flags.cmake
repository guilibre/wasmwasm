if(MSVC)
    add_compile_options(/O2 /DNDEBUG)
else()
    add_compile_options(-O3 -DNDEBUG)
endif()

add_library(compiler_flags INTERFACE)

if(MSVC)
    target_compile_options(compiler_flags INTERFACE
        /W4
        /permissive-
        /w14254
        /w14263
        /w14265
        /w14287
        /w14296
        /w14311
        /w14545
        /w14546
        /w14547
        /w14549
        /w14555
        /w14619
        /w14640
        /w14826
        /w14905
        /w14906
        /w14928
        /we4715
        /we4700
    )
else()
    target_compile_options(compiler_flags INTERFACE
        -Wall
        -Wextra
        -Wpedantic
        -Wshadow
        -Wnon-virtual-dtor
        -Wcast-align
        -Wunused
        -Woverloaded-virtual
        -Wconversion
        -Wsign-conversion
        -Wmisleading-indentation
        -Wduplicated-cond
        -Wduplicated-branches
        -Wlogical-op
        -Wnull-dereference
        -Wdouble-promotion
        -Wformat=2
        -Werror=return-type
        -Werror=uninitialized
    )
endif()
