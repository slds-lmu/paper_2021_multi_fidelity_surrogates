import numpy
import time

import ConfigSpace as CS
from hpbandster.core.worker import Worker
import hpbandster.core.nameserver as hpns
from hpbandster.optimizers import BOHB as BOHB
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
import rpy2.rlike.container as rlc

class branin(Worker):

    def __init__(self, *args, sleep_interval=0, **kwargs):
        super().__init__(*args, **kwargs)

        self.sleep_interval = sleep_interval

        importr('mfsurrogates')
        importr('paradox')
        importr('bbotk')
        fun = robjects.r('cfgs("branin")$get_objective()$eval')

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

        # should be more automatic
        config.update({"fidelity": budget})
        res = numpy.array(fun(rlc.TaggedList(config.values(), tags=config.keys()))[0])
        time.sleep(self.sleep_interval)

        return({
                    'loss': float(res),  # this is the a mandatory field to run hyperband
                    'info': "empty"  # can be used for any user-defined information - also mandatory
                })
    
    @staticmethod
    def get_configspace():
        """
        this will be replaced by something like cfgs("branin")$get_configspace
        """
        config_space = CS.ConfigurationSpace()
        config_space.add_hyperparameter(CS.UniformFloatHyperparameter('x1', lower=0, upper=10))
        config_space.add_hyperparameter(CS.UniformFloatHyperparameter('x2', lower=0, upper=10))
        return(config_space)

"""
# Local and sequential https://automl.github.io/HpBandSter/build/html/quickstart.html
NS = hpns.NameServer(run_id='example1', host='127.0.0.1', port=None)
NS.start()

w = branin(sleep_interval=0, nameserver='127.0.0.1',run_id='example1')
w.run(background=True)

bohb = BOHB(configspace=w.get_configspace(),
            run_id='example1', nameserver='127.0.0.1',
            min_budget=1e-3, max_budget=1)

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

