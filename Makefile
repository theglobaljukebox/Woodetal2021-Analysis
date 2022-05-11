install:
	pip3 install -r requirements.txt
	pip3 install -r validation/requirements.txt
	RScript requirements.R
	
get_data: install
	python3 download_and_format.py
	python3 create_conversion_guide.py
	python3 generate_modal_profiles.py

correlate: 
	@echo Making data...
	RScript correlations/make_embersubsistence.R #
	RScript correlations/make_embersociallayering.R #
	RScript correlations/make_modeldata.R #
	RScript correlations/make_trees.R #
	@echo Building models...
	RScript correlations/line7_models.R # 
	RScript correlations/line10_models.R # 
	RScript correlations/line21_models.R #
	RScript correlations/line23_models.R 
	RScript correlations/line37_models.R
	RScript correlations/pca_models.R
	@echo Making model summary table...
	RScript correlations/model_table.R
	
interrater:
	RScript Inter-rater reliability/GJBIRRPreReg.R
	
validate:
	python3  validation/src/automatic_screening.py
	python3  validation/src/interrater_agreement.py
	
plots: data/full_cantometrics.csv
	python3 plot_maps.py
	RScript supplementary_maps.R