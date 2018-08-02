# title : bayesOptimization
# author : jacob

import pandas as pd
import numpy as np
import warnings
import time
import lightgbm as lgb
from bayes_opt import BayesianOptimization
from sklearn.metrics import roc_auc_score
import sys


def bayesOptimization(X, y, init_round=100, opt_round=100, n_folds=5, random_seed=1, num_iterations=10000, learning_rate=0.02, early_stopping_round=100):
    
    print(">> bayesOptimization start..")
    sys.stdout.flush()
    
    # prepare data
    train_data = lgb.Dataset(data=X, label=y, free_raw_data=False)

    # parameters
    def lgb_eval(max_depth, num_leaves, subsample, colsample_bytree):
        params = dict()
        params['objective'] = 'binary'
        params['num_iterations'] = num_iterations
        params['learning_rate'] = learning_rate
        params['early_stopping_round'] = early_stopping_round
        params['metric'] = 'auc'
        params['max_depth'] = int(round(max_depth)) 
        params["num_leaves"] = int(round(num_leaves))
        params['subsample'] = max(min(subsample, 1), 0)
        params['colsample_bytree'] = max(min(colsample_bytree, 1), 0)
        params['verbose'] = -1
        cv_result = lgb.cv(params, train_data, nfold=n_folds, seed=random_seed, verbose_eval=None, show_stdv=False)
        sys.stdout.flush()
        return max(cv_result['auc-mean'])
        
    tuning_params = {
      'max_depth': (3, 9),
      'subsample': (0.6, 1),
      'colsample_bytree': (0.6, 1),
      'num_leaves': (15, 100)
      }
    
    # range 
    output = BayesianOptimization(lgb_eval, tuning_params, random_state=random_seed, verbose=1)
    
    # optimize
    output.maximize(init_points=init_round, n_iter=opt_round, acq="ucb", kappa= 2.576, xi=0.0)
    
    # print 
    print(output)
    sys.stdout.flush()

    # return best parameters
    return output.res['max']['max_params']
