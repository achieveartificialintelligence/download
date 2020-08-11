#!/usr/bin/env python3

import os
import shutil

resPath = "./temp"

if __name__ == "__main__":

    dirList = []
    path = os.listdir(resPath)
    for p in path:
        shutil.copy(resPath+"/"+p+'/all_results.csv', p+'.csv')
