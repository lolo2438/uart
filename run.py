from vunit import VUnit

vu = VUnit.from_argv()
vu.add_com()
vu.add_verification_components()

lib = vu.add_library("lib")

lib.add_source_files("*.vhd")

lib.set_sim_option("vhdl_assert_stop_level", "failure")

vu.main()
