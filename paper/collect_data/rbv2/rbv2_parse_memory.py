import re
import os
import mmap
from tqdm import tqdm
from functools import partial
import pickle
import pandas as pd
from joblib import Parallel, delayed
import multiprocessing
    
log_file_paths = ["/home/flo/Documents/tiny_projects/slurm-131026.out","/home/flo/Documents/tiny_projects/slurm-131024.out"]
basedir = "/home/flo/Documents/tiny_projects/rng_mem/"
 
def generator():
    while True:
        yield
        
def process_log(log_file, postfix="", offset = 0, chunksize=0):
    regex = br"\[(.*)\.(\d+)\,(classif\..*)\]\[(\d+)\]: ----\[.*\,.*\,\d+,(\d+)\]"
    i = 0
    with open(log_file, "r") as file:
        m = mmap.mmap(file.fileno(), 0, prot=mmap.PROT_READ, offset = offset*mmap.PAGESIZE)
        
        for _ in tqdm(generator()):
   
            line = m.readline()     


            if line == b"":
                print(f"Stopped at offset {offset*mmap.PAGESIZE}")
                break
            
            match = re.match(regex, line, re.S)

            if not match is None:
                tn, tid, lrn, proc = match.group(1,2,3,4) # extract info
                tn = tn[0:40]
                
                lrn_path = "/home/flo/Documents/tiny_projects/rng_mem/"+lrn.decode('utf-8')
                if not os.path.exists(lrn_path):
                    os.mkdir(lrn_path)
                    
                full_path = lrn_path+f"/{tn.decode('utf-8')}_{tid.decode('utf-8')}_{proc.decode('utf-8')}_{postfix}.log"
                if not os.path.exists(full_path):
                    open(full_path, "x")
                    
                with open(full_path, "a") as f:
                    f.writelines(line.decode('utf-8'))
                    f.close()
                    
            i+=1
            if not i % 1e6:
                print(f" iter {i} offset: {m.tell()}")
                debug_log = "/home/flo/Documents/tiny_projects/debug_offsets.log"
                if not os.path.exists(debug_log):
                    open(debug_log, "x")    
                with open(debug_log, "a") as f:
                    f.writelines(f"{offset*mmap.PAGESIZE+m.tell()}\n, iter: {i}")
                    f.close()

        m.close()
        f.close()
        with open(debug_log, "a") as f:
                    f.writelines(f"Finished offset {offset*mmap.PAGESIZE}\n")
                    f.close()
    return None

def read_log(file):
    # time man pages:
    # USAGE: E 4:06.85 K 15.96 U 114.86 P 52% M 417108 kB O 0
    #    %E     Elapsed real time (in [hours:]minutes:seconds).
    #    %e     (Not in tcsh(1).)  Elapsed real time (in seconds).
    #    %S     Total number of CPU-seconds that the process spent in
    #           kernel mode.
    #    %U     Total number of CPU-seconds that the process spent in user
    #           mode.
    #    %P     Percentage of the CPU that this job got, computed as (%U +
    #           %S) / %E.
    #    Memory
    #    %M     Maximum resident set size of the process during its
    #           lifetime, in Kbytes.
    #    %t     (Not in tcsh(1).)  Average resident set size of the
    #           process, in Kbytes.
    #    %K     Average total (data+stack+text) memory use of the process,
    #           in Kbytes.
    #    %P     Percentage of the CPU that this job got, computed as (%U + %S) / %E.
    f = open(file, "r+").read()
    rmem = re.compile("\[(.*)\.(\d+)\,(.*)\]\[(\d+)\]: ----\[.*\,.*\,\d+,(\d+)\] USAGE: E (\d+\:\d+\.\d+) K (\d+\.\d+) U (\d+\.\d+) P (\d+)\% M (\d+) kB .*")
    rseed = re.compile("\[(.*)\.(\d+)\,(.*)\]\[(\d+)\]: ----\[.*\,.*\,\d+,(\d+)\] .* Evaluating seed (\d+)")
    outputs = {}
    for match in rmem.finditer(f):
        t, tid, lrn, core, idx, E, K, U, P, M =  match.group(1,2,3,4,5,6,7,8,9,10)
        outputs[idx] = {"Task":t, "DataID":tid, "E": E, "K": K, "U":U, "P":P, "M":M}
    for match in rseed.finditer(f):
        t, tid, lrn, core, idx, seed =  match.group(1,2,3,4,5,6)
        if idx in outputs.keys():
            outputs[idx].update({"Seed":seed})
            
    if not (len(outputs)): # Break if no results
        return None

    df = pd.DataFrame.from_dict(outputs, orient = "index")
    # import pdb; pdb.set_trace()
    pd.to_numeric(df.DataID)
    pd.to_numeric(df.K)
    pd.to_numeric(df.U)    
    pd.to_numeric(df.P)
    pd.to_numeric(df.M)    
    pd.to_numeric(df.Seed)
    df.to_csv(basedir+lrn+".csv", mode = "a", index=False, header = False)
    
    return None

def collect_csv():
    names = ["task", "data_id", "E", "K", "U", "P", "M", "seed"]
    
    # xgboost
    df1 = pd.read_csv(basedir+'classif.xgboost.dart.csv', names = names)
    df1[["setting"]] = "dart"
    df2 = pd.read_csv(basedir+'classif.xgboost.gblinear.csv', names = names)
    df2[["setting"]] = "gblinear"
    df3 = pd.read_csv(basedir+'classif.xgboost.gbtree.csv', names = names)
    df3[["setting"]] = "gbtree"
    df = df1.append(df2).append(df3)
    df["learner_id"] = "classif.xgboost"
    df = df.drop_duplicates()
    df.to_csv(basedir+'classif.xgboost_final.csv')
    
    df1 = pd.read_csv(basedir+'classif.svm.csv', names = names)
    df1[["setting"]] = "all"
    df2 = pd.read_csv(basedir+'classif.svm.radial.csv', names = names)
    df2[["setting"]] = "radial"
    df = df1.append(df2)
    df["learner_id"] = "classif.svm"
    df.to_csv(basedir+'classif.svm_final.csv')
    
    
    df = pd.read_csv(basedir+'classif.rpart.csv', names = names)
    df["learner_id"] = "classif.rpart"
    df.to_csv(basedir+'classif.rpart_final.csv')
    
    df = pd.read_csv(basedir+'classif.glmnet.csv', names = names)
    df["learner_id"] = "classif.glmnet"
    df.to_csv(basedir+'classif.glmnet_final.csv')
    
    df = pd.read_csv(basedir+'classif.ranger.pow.csv', names = names)
    df["learner_id"] = "classif.ranger"
    df.to_csv(basedir+'classif.ranger_final.csv')
    
    df = pd.read_csv(basedir+'classif.RcppHNSW.csv', names = names)
    df["learner_id"] = "classif.RcppHNSW"
    df.to_csv(basedir+'classif.RcppHNSW_final.csv')
    
    df = pd.read_csv(basedir+'classif.kerasff.csv', names = names)
    df["learner_id"] = "classif.kerasff"
    df.to_csv(basedir+'classif.kerasff_final.csv')
    


# 64 329 698

if __name__ == __name__=='__main__':
    if not os.path.exists(basedir):
        os.mkdir(basedir)  
    # Separate two log-files into files per learner / task
    # process_log(log_file_paths[0], "0")
    # proclog = partial(process_log, log_file_paths[1], "1")
    # proclog(offset = 0)
    # offsets = [x*400000 for x in range(12)]
    # Parallel(n_jobs=8)(delayed(proclog)(i) for i in offsets)
    # print("Finished preproc step 1/2")
    
    # lrn = os.listdir(basedir)
    # logs = []
    # for l in lrn:
    #     log = os.listdir(basedir+l)
    #     logs = logs + [basedir+l+"/"+x for x in log]    
    # print(len(logs))
    
    # import random
    # random.shuffle(logs)
    # inputs = tqdm(logs)
    # Parallel(n_jobs=4)(delayed(read_log)(i) for i in inputs)
    # print("Finished preproc step 2/2")
    
    collect_csv()