
import pandas as pd
from sksurv.datasets import load_veterans_lung_cancer

data_x, data_y = load_veterans_lung_cancer()
print(pd.DataFrame.from_records(data_y[[11, 5, 32, 13, 23]], index=range(1, 6)))


# Kaplan-Meier

import matplotlib.pyplot as plt
from sksurv.nonparametric import kaplan_meier_estimator

time, survival_prob, conf_int = kaplan_meier_estimator(

    data_y['Status'], data_y['Survival_in_days'], conf_type='log-log'
)

plt.step(time, survival_prob, where='post')
plt.fill_between(time, conf_int[0], conf_int[1], alpha=0.25, step='post')
plt.ylim(0, 1)
plt.ylabel('est.probability of survival $\hat{S}(t)$')
plt.xlabel('time $t$')