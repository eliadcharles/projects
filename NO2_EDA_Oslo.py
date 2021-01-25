#packages used
import numpy as np
import scipy.stats as st
import matplotlib.pyplot as plt
import seaborn as sns
import scipy as sp
import pandas as pd

sns.set_style("whitegrid",{'axes.grid' : False})
from statsmodels.distributions.empirical_distribution import ECDF

import statsmodels.api as sm
import plotly.express as px
import plotly.io as pio
import plotly.graph_objects as go
import plotly.offline as py

#reading data into numpy and pandas dataframes:Producing Table2
Raw_Data = open('NO2Data.txt','r')
NO2Data = np.loadtxt(Raw_Data,unpack=False)
SUB = str.maketrans("0123456789", "₀₁₂₃₄₅₆₇₈₉")


column_names= ['log(NO2)'.translate(SUB),'Cars/hour','Temp 2m (deg.C)',
               'Wind (m/s)','TempDif(deg.C)','Wind Dir(degrees)','Hours','Day']
#filtering outliers and dropping NaN (outlier=3sds outside mean)
z_score=np.abs(st.zscore(NO2Data,axis=1))
Filter = (z_score<3).all(axis=1)
NO2Data=NO2Data[Filter]

dfNO2Data = pd.DataFrame(NO2Data, columns=column_names)
dfNO2Data=dfNO2Data.dropna()

Table2 = (dfNO2Data.describe())
Table2.to_excel("Table2.xlsx")



#producing figure 1. Pair plot of all attributes except Day
plt.figure(figsize=(20,20))

for a in range (0,7):
    for b in range(0,7):
        plt.subplot(7,7,1+a+(7*b))
        if a==b:
            sns.kdeplot(NO2Data[:,a], color='orange',label="Kernel Density")
            sns.histplot(NO2Data[:,a],stat='density', color='lightblue',label="Histogram")
            plt.xlabel(column_names[a])
            plt.ylabel('Density')
        else:
            if a<b:
                sns.kdeplot(x=np.ravel(NO2Data[:,a]),y=np.ravel(NO2Data[:,b]),cmap="Blues")
            else:
                plt.scatter(NO2Data[:,a],NO2Data[:,b],c='r',marker='.')
            plt.ylabel(column_names[b])
            plt.xlabel(column_names[a])

plt.tight_layout()
plt.savefig('Figures/Figure8.pdf')

plt.show()

#NO2 chosen for univariate analyis: unpacking
NO2,car,Temp2,WindS,TempDif,WindD,Hour,Day = [NO2Data[:,i] for i in range(0,8)]

#Calculating mode from histogram
def histmode(x):
    hist, bin_edges = np.histogram(x,density=True,bins=50)
    plt.step(bin_edges,np.concatenate(([0.],hist)),color='lightblue')
    ax=plt.gca()
    line= ax.lines[0]
    XVals =line.get_xdata()
    YVals =line.get_ydata()
    return float(XVals[np.argmax(YVals)])


#Calculating mode from KDE
#source: https://towardsdatascience.com/modal-series-1-how-to-estimate-the-global-mode-from-data-sample-e3e41380bfdb
from scipy.stats import mode, gaussian_kde
from scipy.optimize import minimize, shgo

def kde(array, cut_down=True, bw_method='scott'):
    if cut_down:
        bins, counts = np.unique(array, return_counts=True)
        f_mean = counts.mean()
        f_above_mean = bins[counts > f_mean]
        bounds = [f_above_mean.min(), f_above_mean.max()]
        array = array[np.bitwise_and(bounds[0] < array, array < bounds[1])]
    return gaussian_kde(array, bw_method=bw_method)

def refined_mode_estimation(array, cut_down=True, bw_method='scott'):
    kernel = kde(array, cut_down=cut_down, bw_method=bw_method)
    height = kernel.pdf(array)
    x0 = array[np.argmax(height)]
    span = array.max() - array.min()
    dx = span / 4
    bounds = np.array([[x0 - dx, x0 + dx]])
    linear_constraint = [{'type': 'ineq', 'fun': lambda x:  x - 0.5}]
    results = minimize(lambda x: -kernel(x)[0], x0=x0, bounds=bounds, constraints=linear_constraint)
    return results.x[0]

KDE_ModeEstimation = refined_mode_estimation(NO2)
KDE_ModeEstimation

#Summary of Moments
NO2u= np.mean(NO2)
NO2median= np.median(NO2)
NO2var=np.var(NO2)
NO2sd=np.sqrt(np.var(NO2))
NO2Skew = sp.stats.moment(NO2,3)
NO2Kurtosis = sp.stats.moment(NO2,4)


moments =[NO2u,NO2median,,NO2var,NO2sd,NO2Skew,NO2Kurtosis,histmode(NO2),KDE_ModeEstimation]


momentsdf= pd.DataFrame(moments, columns=['log(NO2)'.translate(SUB)],
index=['Mean','Median','Variance','Standard Deviation',
'Skew','Kurtosis','Histogram-Mode','KDE-Mode'
])
momentsdf.to_excel("Table3.xlsx")

#DAgostino's K-squared Test: test's whether samples are normally distributed.
from scipy.stats import normaltest
for i in range(0,5):
    data = NO2Data[:,i]
    stat, p = normaltest(data)
    print('stat=%.3f, p=%.3f' % (stat, p))
    if p > 0.05:
        print(column_names[i],'Probably Gaussian')
    else:
        print(column_names[i],'Probably not Gaussian')


#bootstrapping
def summariseSample(x, conf=95):
    alpha = 100 - conf
    lower = np.percentile(x, alpha/2)
    median = np.median(x)
    upper = np.percentile(x, 100 - alpha/2)
    return( lower, median, upper )


listofVals = np.sort(NO2)
samples=1000
bins = 50
sampleSize= int(len(NO2))
bootmatrix = np.zeros((samples,bins))

for x in range(0,samples):
    sample = np.random.choice(listofVals, size= sampleSize, replace=True)
    NO2_hist, crntbinedges  = np.histogram(sample, bins= bins,density=True)
    bootmatrix[x,:] = NO2_hist

myConf = 95 # a percentage
confBandMat = np.zeros((3, bins)) #(row, column)

for i in range(0, bins):
    x = summariseSample(bootmatrix[:,i], myConf )#calculate confidence intervals for each bin.
    confBandMat[:,i] = x

lower, medium, upper = [confBandMat[j,:] for j in range(0,3)]

#percentiles
N25=np.percentile(NO2,25)
N50=np.percentile(NO2,50)
N75=np.percentile(NO2,75)
N95=np.percentile(NO2,95)
N99=np.percentile(NO2,99)

#Plotting figure2
plt.figure(figsize=(20,20))
xx = np.ones(2)
yy = np.array([0,1])

plt.subplot(2,2,1)
KDE = sns.kdeplot(NO2,kernel='gau',shade=True,bw='silverman')
plt.plot(NO2u*xx,yy,'--g',label='Mean')
plt.plot(NO2median*xx,yy,'--b',label='Median')
plt.plot(KDE_ModeEstimation*xx,yy,'--r', label='KDE-estimated Mode')
plt.plot(mode*xx,yy,color='orange',label='Hist-Estimated Mode')
plt.xlabel('log(NO2)'.translate(SUB),fontsize=14)
plt.xlim([0,7])
plt.ylim([0,0.8])
plt.legend(loc=2, prop={'size': 15})

plt.subplot(2,2,2)
CDF = sns.kdeplot(NO2, cumulative=True)
plt.plot([0,N25,N25],[0.25,0.25,0],'--k')
plt.plot([0,N50,N50],[0.50,0.5,0],'--k')
plt.plot([0,N75,N75],[0.75,0.75,0],'--k')
plt.plot([0,N99,N99],[0.99,0.99,0],'--k')
plt.xlim([0,1])
plt.ylim([0,1])
plt.xticks((0,N25,N50,N75,N95,N99),('0','$P_{25}$','$P_{50}$','$P_{75}$','$P_{95}$','$P_{99}$'))
plt.xlabel('log(NO2)'.translate(SUB),fontsize=14)


plt.subplot(2,2,3)
plt.step(ecdf.x, ecdf.y, color='red')
plt.plot([0,N25,N25],[0.25,0.25,0],'--k')
plt.plot([0,N50,N50],[0.50,0.5,0],'--k')
plt.plot([0,N75,N75],[0.75,0.75,0],'--k')
plt.plot([0,N99,N99],[0.99,0.99,0],'--k')
plt.xlim([0,1])
plt.ylim([0,1])
plt.xticks((0,N25,N50,N75,N95,N99),('0','$P_{25}$','$P_{50}$','$P_{75}$','$P_{95}$','$P_{99}$'))
plt.xlabel('Log(NO2))'.translate(SUB),fontsize=14)
plt.ylabel('ECDF/100',fontsize=14)

plt.subplot(2,2,4)
plt.step(bin_edges2,np.concatenate(([0.],hist2)),color='lightblue', label='Values',linewidth=4)
plt.step(bin_edges2,np.concatenate(([0.],lower)),color='red',label='lower bound')
plt.step(bin_edges2,np.concatenate(([0.],medium)),color='green', label='median')
plt.step(bin_edges2,np.concatenate(([0.],upper)),color='blue',label='upper bound')
plt.legend(loc=2, prop={'size': 15})
plt.ylabel('Density',fontsize=14)
plt.xlabel('Log(NO2)'.translate(SUB),fontsize=12)

plt.tight_layout()

#Multivariate analyis

#Correlation Matrix
dfNO2Data.corr()

Table4 = (dfNO2Data.corr())
Table4.to_excel("Table4.xlsx")


#3dplotting
fig = px.scatter_3d(dfNO2Data, x='Cars per hour', y= 'log(NO2) ppm', z='Hour',
                    color='Hour')
fig.show()
