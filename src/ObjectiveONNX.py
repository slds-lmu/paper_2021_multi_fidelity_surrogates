import onnxruntime
from rpy2.robjects.packages import importr
import rpy2.rlike.container as rlc
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri # install any dependency package if you get error like "module not found"
import pandas as pd
import numpy as np
from ConfigSpace.read_and_write import json
pandas2ri.activate()

#class ObjectiveONNX:
#    def __init__(self, model_path, trafo_dict, task = NULL, active_session = True):
#        self.trafo_dict = trafo_dict
#        base = importr("base")
#        trafo_dict = base.readRDS(trafo_dict)
#
#    def fun(self, x):
#        # Re-order columns in case the order changed through trafos.
#
#    def eval(self):
#        session = onnxruntime.InferenceSession("../attic/multifidelity_data/nb301/model.onnx")

# nb301 exmaple
with open('configspaces/configspace_nb301.json', 'r') as f:
  jason_string = f.read()
  config = json.read(jason_string)

xdt = config.sample_configuration(1).get_dictionary()
xdt["epoch"] = 50
xdt = pd.DataFrame.from_dict([xdt])
xdt = pandas2ri.py2rpy(xdt)

base = importr("base")
trafo_dict = base.readRDS("../attic/multifidelity_data/nb301/dicts.rds")
param_set = base.readRDS("../attic/multifidelity_data/nb301/param_set.rds")

mfsurrogates = importr("mfsurrogates")

li_ = mfsurrogates.convert_for_onnx(xdt, param_set = param_set, trafo_dict = trafo_dict)
li = { key : li_.rx2(key) for key in li_.names }
li["continuous"] = np.atleast_2d(li["continuous"]).astype("float32")

session = onnxruntime.InferenceSession("../attic/multifidelity_data/nb301/model.onnx")
session.run(None, li)

