add_executable(simple_1d simple_1d.f90)
target_link_libraries(simple_1d neasyf::neasyf)

add_executable(simple_xy_wr simple_xy_wr.f90)
target_link_libraries(simple_xy_wr neasyf::neasyf)
