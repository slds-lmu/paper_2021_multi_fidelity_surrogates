from yahpo_gym.configuration import *
from yahpo_gym import BenchmarkSet
from os.path import exists
import json
from joblib import Parallel, delayed

def L(x):
    return x if type(x) == list else [x]

def n_instances(b):
    return {'n_instances': len(b.instances)}

def per_instance(cc, df):
    if cc.instance_names in df.columns:
        tmp = df[cc.instance_names].value_counts()
        out = tmp.agg({'min_per_task': 'min', 'median_per_task': 'median', 'max_per_task': 'max'}).astype(int).to_dict()
        out.update({'full_per_task': tmp.to_dict()})
        return out
    else:
        n = df.shape[0]
        return {'min_per_task': n, 'median_per_task': n, 'max_per_task': n, 'full_per_task': {'data': n}}

def write_json(dct, out):
    with open(out, 'w') as f:
        json.dump(dct, f, indent=4)

def dump_data(key, overwrite = False):
        if key in ['fcnet']:
            return None

        cc = cfg(key)
        out_path = f'{cc.config_path}/scenario_stats.json'

        if exists(out_path) and not overwrite:
            with open(out_path, 'r') as f:
                return json.load(f)

        print(f'Dumping {key}')

        b = BenchmarkSet(key)
        out_path = f'{cc.config_path}/scenario_stats.json'
        dct = {
            'instance': key,
            'n_targets' : len(cc.y_names),
            'n_categorical' : len(set(cc.cat_names)-set(L(cc.instance_names))), 
            'n_numeric' : len(set(cc.cont_names)-set(L(cc.instance_names))),
            'fidelity' : ','.join(cc.fidelity_params)
        }
        dct.update(n_instances(b))

        df = cc.data
        # Only use instances in ConfigSpace
        if cc.instance_names in df.columns:
            df = df[df[cc.instance_names].astype('str').isin([str(x) for x in b.instances])]
        
        dct.update(per_instance(cc, df))

        write_json(dct, out_path)

        return dct

if __name__ == '__main__':
    dcts = Parallel(n_jobs=4)(delayed(dump_data)(k) for k in cfg().configs.keys())
    print(dcts)

    import pandas as pd
    df = pd.DataFrame.from_dict([x for x in dcts if x is not None])
    print(df)
    df.drop(['full_per_task'], axis=1, inplace=True)
    df.to_csv('scenario_stats.csv')




