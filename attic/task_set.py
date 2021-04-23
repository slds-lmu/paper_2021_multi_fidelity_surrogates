import pickle as cPickle
import numpy as np
from matplotlib import pylab as plt
import tensorflow.compat.v2 as tf # for gfile.
from concurrent import futures
import tqdm
import json
import os

gfile = tf.io.gfile

def load_joint_cache(task, opt_set_name):
  """Loads the learning curves for the given task and opt_set_name."""
  base_dir = "gs://task_set_data/task_set_data/"
  p = os.path.join(base_dir, task,
                    "%s_10000_replica5.npz" % (opt_set_name))
  cc = np.load(gfile.GFile(p, "rb"))
  return cc["optimizers"], cc["xs"], cc["ys"]

def threaded_tqdm_map(threads, func, data):
  """Helper that does a map on multiple threads."""
  future_list = []
  with futures.ThreadPoolExecutor(threads) as executor:
    for l in tqdm.tqdm(data, position=0):
      future_list.append(executor.submit(func, l))
    return [x.result() for x in tqdm.tqdm(future_list, position=0)]


def load_tasks(tasks):
  """Multi threaded loading of all data for each task.
  Args:
    tasks: list of task names
  Returns:
    A dictionary mapping taks name to tuples of:
    (optimizer names, x data points, and y data points)
  """
  def load_one(t):
    adam8p = load_joint_cache(t, "adam8p_wide_grid_1k")
    adam6p = load_joint_cache(t, "adam6p_wide_grid_1k")
    adam4p = load_joint_cache(t, "adam4p_wide_grid_1k")
    adam1p = load_joint_cache(t, "adam1p_wide_grid_1k")
    nadamw = load_joint_cache(t, "nadamw_grid_1k")
    return {
      "adam8p_wide_grid_1k": adam8p,
      "adam6p_wide_grid_1k": adam6p,
      "adam4p_wide_grid_1k": adam4p,
      "adam1p_wide_grid_1k": adam1p,
      "nadamw": nadamw,
    }
  
  results = threaded_tqdm_map(100, load_one, tasks)
  
  for k,v in zip(tasks, results):
    if v is None:
      print("No data found for task: %s"%k)
      
  return {k:v for k, v in zip(tasks, results) if v is not None}

def get_task_names():
  base_dir = "gs://task_set_data/task_set_data/"
  tasks = [t.replace("/", "") for t in gfile.listdir(base_dir) if "$folder$" not in t]
  return sorted(tasks)