add_library(ObsOperator STATIC EXCLUDE_FROM_ALL
    obsoperator_mod.F90
    obsoperator_entry_mod.F90
    obsoperator_sample_mod.F90
    obsoperator_input_mod.F90
)
target_link_libraries(ObsOperator
	PUBLIC GeosUtil
)