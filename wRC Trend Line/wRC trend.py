#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 23 20:24:27 2023

@author: tysonking
"""

import matplotlib.animation as anim
import matplotlib.pyplot as plt
import pandas as pd
import os

os.chdir('/Users/tysonking/Desktop/Projects/softball-projects/wRC Trend Line')

stats = pd.read_csv("wRC by week NCSU.csv", delimiter=',').sort_values(by=['Player','through'])

def update(frame_number, plot, group_var):
    # Subset the data for the current frame and group
    data = stats.groupby(group_var, level=0).apply(lambda x: x.iloc[:frame_number+1])
    # Iterate over each group and update the corresponding line on the plot
    for group_name, group_data in data.groupby(group_var):
        line = plot[group_name]
        line.set_data(group_data['through'], group_data['wRC'])
        
fig, ax = plt.subplots()
plot = {}
for group_name, group_data in stats.groupby('Player'):
    line = ax.plot(group_data.through, group_data.wRC, label=group_name)
    plot[group_name] = line

#animation = anim.FuncAnimation(fig, update, frames=len(stats), fargs=(plot, 'Player'), interval=25)

#plt.rcParams['animation.ffmpeg_path'] = '/usr/local/bin/ffmpeg'
#animation.save('example_animation.mp4')

plt.show()