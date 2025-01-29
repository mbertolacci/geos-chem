add_library(ObsOperator STATIC EXCLUDE_FROM_ALL
    obsoperator_mod.F90
    obsoperator_entry_mod.F90
    obsoperator_sample_mod.F90
    obsoperator_input_alt_mod.F90
    obsoperator_output_mod.F90
    yaml_scanner_mod.F90
    yaml_parser_mod.F90
    yaml_document_mod.F90
    yaml_node_mod.F90
)
target_link_libraries(ObsOperator
	PUBLIC GeosUtil
)