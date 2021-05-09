#from ConfigSpace import ConfigurationSpace
#from ConfigSpace.read_and_write import json
#
#class BenchmarkConfig:
#    def __init__(self, id, download_url, workdir, model_name, config_space_file = None, data_file, dicts_file, keras_model_file, onnx_model_file, target_variables, codomain):
#        self.id = id
#        self.download_url = download_url
#        self.workdir = workdir
#        self.model_name = model.name
#        self.subdir = self.workdir + self.model_name
#        self.configspace_file = configspace_file
#        self.data_file = data_file
#        self.dicts_file = dicts_file
#        self.keras_model_file = keras_model_file
#        self.onnx_model_file = onnx_model_file
#        self.target_variables = target_variables
#        self.codomain = codomain
#
#    def setup():
#        print("FIXME")
#
#    def get_objective(self, task = None, target_variables = None):
#        codomain = self.codomain
