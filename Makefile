

analysis:
	@echo Making data...
	RScript correlations/make_embersubsistence.R
	RScript correlations/make_modeldata.R
	@echo Building models...
	RScript correlations/line7_models.R
	RScript correlations/line10_models.R
	RScript correlations/line21_models.R
	RScript correlations/line23_models.R
	RScript correlations/line37_models.R
	RScript correlations/pca_models.R
	@echo Making model summary table...
	RScript correlations/model_table.R