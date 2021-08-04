

analysis:
	@echo Making data...
	RScript make_emberproductivity.R
	RScript make_modeldata.R
	@echo Building models...
	RScript line3_models.R
	RScript line10_models.R
	RScript line21_models.R
	RScript line23_models.R
	RScript line37_models.R
	RScript pca_models.R
	@echo Making model summary table...
	