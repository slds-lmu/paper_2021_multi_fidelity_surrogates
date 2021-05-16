import time
import numpy as np
import pandas as pd
from ConfigSpace.read_and_write import json
import onnxruntime
from rpy2.robjects.packages import importr
import rpy2.rlike.container as rlc
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
from hpbandster.core.worker import Worker
import hpbandster.core.nameserver as hpns
from hpbandster.optimizers import BOHB as BOHB

class nb301(Worker):

    def __init__(self, *args, sleep_interval=0, **kwargs):
        super().__init__(*args, **kwargs)

        self.sleep_interval = sleep_interval
        base = importr("base")
        self.mfsurrogates = importr("mfsurrogates")
        self.session = onnxruntime.InferenceSession("attic/multifidelity_data/nb301/model.onnx")
        self.param_set = base.readRDS("attic/multifidelity_data/nb301/param_set.rds")
        self.trafo_dict = base.readRDS("attic/multifidelity_data/nb301/dicts.rds")
        self.target_names = ["val_accuracy", "runtime"]

        pandas2ri.activate()

    def compute(self, config, budget, **kwargs):
        """
        Args:
            config: dictionary containing the sampled configurations by the optimizer
            budget: (float) amount of time/epochs/etc. the model can use to train

        Returns:
            dictionary with mandatory fields:
                'loss' (scalar)
                'info' (dict)
        """

        config.update({"epoch": int(budget)})  # FIXME: budget trafo to match epoch range and int
        xdt = pd.DataFrame.from_dict([config])
        xdt = pandas2ri.py2rpy(xdt)

        li_ = self.mfsurrogates.convert_for_onnx(xdt, param_set = self.param_set, trafo_dict = self.trafo_dict)
        li = { key : li_.rx2(key) for key in li_.names }
        li["continuous"] = np.atleast_2d(li["continuous"]).astype("float32")
        res = self.session.run(None, li)[0]
        res = self.mfsurrogates.retrafo_predictions(res, param_set = self.target_names, trafo_dict = self.trafo_dict)
        time.sleep(self.sleep_interval)

        return({
                    'loss': float(res[0,0]),  # this is the a mandatory field to run hyperband
                    'info': "empty"  # can be used for any user-defined information - also mandatory
                })

    @staticmethod
    def get_configspace():
        with open('src/configspaces/configspace_nb301_drop_epoch.json', 'r') as f:
            json_string = f.read()
            cs = json.read(json_string)
        return(cs)

"""
# Local and sequential https://automl.github.io/HpBandSter/build/html/quickstart.html
NS = hpns.NameServer(run_id='example1', host='127.0.0.1', port=None)
NS.start()

w = nb301(sleep_interval=0, nameserver='127.0.0.1',run_id='example1')
w.run(background=True)

bohb = BOHB(configspace=w.get_configspace(),
            run_id='example1', nameserver='127.0.0.1',
            min_budget=1, max_budget=50)

res = bohb.run(n_iterations=1)

bohb.shutdown(shutdown_workers=True)
NS.shutdown()

id2config = res.get_id2config_mapping()
incumbent = res.get_incumbent_id()

print('Best found configuration:', id2config[incumbent]['config'])
print('A total of %i unique configurations where sampled.' % len(id2config.keys()))
print('A total of %i runs where executed.' % len(res.get_all_runs()))
print('Total budget corresponds to %.1f full function evaluations.'%(sum([r.budget for r in res.get_all_runs()])/1))
"""

