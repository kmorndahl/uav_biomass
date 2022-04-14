import argparse

class createRasterStackParser(object):
    """
    Parser object for 1_2_createRasterStack.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to stack UAV based imagery, for use as predictors in classification")

        self.parser.add_argument("site",
                                 type=str,
                                 help="site")

    def __repr__(self):
        return "<Parser object for 1_2_createRasterStack.py>"

class runCV_ClassificationParser(object):
    """
    Parser object for 2_1_hyperparameterTuning_nestedCV_Classification.py and 2_2_accuracyAssessment_CV_Classification.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to run cross validation (select hyperparameters and perform accuracy assessment) for random forest classification models to predict plant functional types across UAV imagery")

        self.parser.add_argument("site",
                                 type=str,
                                 help="site")
        
        self.parser.add_argument("cv_split_num",
                            type=int,
                            help="outer cross validation split number")

    def __repr__(self):
        return "<Parser object for 2_1_hyperparameterTuning_nestedCV_Classification.py and 2_2_accuracyAssessment_CV_Classification.py>"

class buildRF_ClassificationParser(object):
    """
    Parser object for 2_4_buildFinalRFmodels_Classification.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to create random forest classification models to predict plant functional types across UAV imagery")

        self.parser.add_argument("site",
                                 type=str,
                                 help="site")

    def __repr__(self):
        return "<Parser object for 2_4_buildFinalRFmodels_Classification.py>"

class featureImportance_ClassificationParser(object):
    """
    Parser object for 2_5_featureImportance.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to assess feature importance from UAV classification models")

        self.parser.add_argument("site",
                                 type=str,
                                 help="site")

    def __repr__(self):
        return "<Parser object for 2_5_featureImportance.py>"

class classifyParser(object):
    """
    Parser object for 3_0_classify.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to apply saved Random Forest model to raster stack to create classified raster")

        self.parser.add_argument("raster_infile",
                                 type=str,
                                 help="input raster stack file name")
        self.parser.add_argument("model_infile",
                                 type=str,
                                 help="pickle file with saved model")
    def __repr__(self):
        return "<Parser object for 3_0_classify.py>"

class fillCHMParser(object):
    """
    Parser object for 5_1_createCHMs_RF.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to fill existing CHM holes with predictions from random forest model")

        self.parser.add_argument("site_name",
                                 type=str,
                                 help="site name")
    def __repr__(self):
        return "<Parser object for 5_1_createCHMs_RF.py>"

class modelComparison_UAVtoLSparser(object):
    """
    Parser object for 21_modelComparisonUAVtoLS.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to use LazyPredict to compare regressor models for predicting biomass at Landsat scale")
        
        self.parser.add_argument("cv_split_num",
                            type=int,
                            help="outer cross validation split number")

    def __repr__(self):
        return "<Parser object for 21_modelComparisonUAVtoLS.py>"

class runCV_UAVtoLSparser(object):
    """
    Parser object for 22_runCV_UAVtoLS.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to create random forest regressor models to predict biomass at Landsat scale")

        self.parser.add_argument("pft",
                                 type=str,
                                 help="pft")
        
        self.parser.add_argument("cv_split_num",
                            type=int,
                            help="outer cross validation split number")

    def __repr__(self):
        return "<Parser object for 22_runCV_UAVtoLS.py>"

class hyperparamCurves_UAVtoLSparser(object):
    """
    Parser object for 22_hyperparametersValidationCurve_UAVtoLS.py
    """

    def __init__(self):
        self.parser = argparse.ArgumentParser(description="Script to create hyperparamter validation curves for models that predict biomass at Landsat scale")

        self.parser.add_argument("pft",
                                 type=str,
                                 help="pft")
                                 
    def __repr__(self):
        return "<Parser object for 22_hyperparametersValidationCurve_UAVtoLS.py>"