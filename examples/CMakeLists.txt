add_executable(example1 example.f90)
target_link_libraries(example1 neasyf::neasyf)

add_executable(simple_xy_wr simple_xy_wr.f90)
target_link_libraries(simple_xy_wr neasyf::neasyf)
