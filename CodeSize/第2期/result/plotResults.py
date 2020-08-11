#!/usr/bin/env python3
#coding: utf-8
# by Sun
import sys
import os
import csv
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('Agg')

numOfHeader = 6
testOrder = [
    [(1, 2), (3, 4), (5, 6), (7, 8)],
    [(1, 3), (2, 4), (5, 7), (6, 8)],
    [(1, 5), (2, 6), (3, 7), (4, 8)]
]
orderName = [
    "o3os -> os",
    "64 -> 32",
    "GCC -> clang"
]


class VerFile:

    def __init__(self, filename=""):
        self.filename = ""
        self.relatList = []
        self.itemDict = {}
        if filename != "":
            self.filename = filename[:-4]

            curFile = csv.reader(open(filename))
            i = 1
            for line in curFile:
                i += 1
                if i > numOfHeader:
                    if len(line) == 3:
                        if self.itemDict.get(line[0]):
                            self.itemDict[line[0]] += int(line[2])
                        else:
                            self.itemDict[line[0]] = int(line[2])
                    else:
                        print("WARNING!", self.filename, " Line ",
                              i+numOfHeader, " is wrong!")

    def getKeys(self):
        return self.itemDict.keys()

    def getValue(self, key=""):
        if key == "":
            return self.itemDict.values()
        else:
            return self.itemDict[key]

    def setRelatPercent(self, numList):
        self.relatList = numList

    def getRelatPercent(self):
        return self.relatList

    def getFilename(self):
        return self.filename


def getALLCSV(path="."):
    files = os.listdir(path)
    csvFiles = []
    for f in files:
        if len(f) >= 5 and f[-4:] == ".csv":
            csvFiles.append(f)
    return csvFiles


def toPercent(verList, order):
    testFiles = []
    for t in order:
        relativeNum = list(verList[t[0]-1].getValue())
        dividedNum = list(verList[t[1]-1].getValue())
        valueList = []

        for i in range(0, len(relativeNum)):
            valueList.append(dividedNum[i] / relativeNum[i] * 100)
        verList[t[1]-1].setRelatPercent(valueList)
        testFiles.append(verList[t[1]-1])

    return testFiles


def plotFigure(testFiles, fn):
    # 并列柱状图

    x = np.arange(len(verFiles[0].getKeys()))  # 柱状图在横坐标上的位置
    tick_label = verFiles[0].getKeys()
    # 列出你要显示的数据，数据的列表长度与x长度相同

    i = -len(testFiles)/2
    for vf in testFiles:
        i += 1
        y = vf.getRelatPercent()

        bar_width = 0.9 / len(testFiles)  # 设置柱状图的宽度
        plt.title(fn)

        # 绘制并列柱状图
        plt.bar(x + bar_width*i, y, bar_width,
                label=vf.getFilename())
        plt.legend(loc = 4)  # 显示图例，即label
        # 显示x坐标轴的标签,即tick_label,调整位置，使其落在两个直方图中间位置
        plt.xticks(x+bar_width/len(testFiles), tick_label,
                   rotation=90, position=(0, 0))
    # plt.show()
    plt.tight_layout()
    plt.savefig("figure_"+str(fn)+".png")


if __name__ == "__main__":
    fileName = sys.argv[1]
    order = []

    for i in range(2, len(sys.argv), 2):
        tu = (int(sys.argv[i]), int(sys.argv[i+1]))
        order.append(tu)

    fileList = getALLCSV()
    fileList.sort()
    xVals = []
    verFiles = []
    for f in fileList:
        verFiles.append(VerFile(f))

    for i in verFiles:
        print(i.getFilename())

    testFiles = toPercent(verFiles, order)

    plotFigure(testFiles, fileName)
