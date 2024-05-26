import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


def CalculateShareRatios(Data, TopX, BottomY):
    IncomeData = Data
    SortedData = IncomeData.sort_values(ascending=False).reset_index(drop=True)

    TopCutoff, BottomCutoff = int(len(SortedData) * TopX), int(len(SortedData) * BottomY)
    TopData, BottomData = SortedData[:TopCutoff], SortedData[-BottomCutoff:]
    TopXIncome, BottomYIncome = TopData.sum(), BottomData.sum()
    TopXShare, BottomYShare = TopXIncome / IncomeData.sum() * 100, BottomYIncome / IncomeData.sum() * 100

    print(f'Dataset: {Data.name}\n'
          f'S{(1 - TopX) * 100:.0f}/S{BottomY * 100:.0f} Ratio: {TopXIncome / BottomYIncome:.2f}\n'
          f'Top {TopX * 100:.0f} Share of Income: {TopXShare:.2f}%\n'
          f'Bottom {BottomY * 100:.0f} Share of Income: {BottomYShare:.2f}%\n')


def CalculatePercentileRatios(Data, TopX, BottomY):
    SortedData = Data.sort_values(ascending=True).reset_index(drop=True)

    TopPercentileIndex = int((1 - TopX) * len(SortedData)) - 1
    BottomPercentileIndex = int(BottomY * len(SortedData)) - 1

    TopPercentileValue = SortedData.iloc[TopPercentileIndex]
    BottomPercentileValue = SortedData.iloc[BottomPercentileIndex]
    PercentileRatio = TopPercentileValue / BottomPercentileValue

    print(f'Dataset: {Data.name}\n'
          f'P{(1 - TopX) * 100:.0f}/P{BottomY * 100:.0f} Ratio: {PercentileRatio:.2f}\n'
          f'Income of the {(1 - TopX) * 100:.0f}th Percentile Family: {TopPercentileValue:.2f}\n'
          f'Income of the {BottomY * 100:.0f}th Percentile Family: {BottomPercentileValue:.2f}\n')


def CalculateGINI(Data):
    IncomeData = Data
    SortedData = IncomeData.sort_values(ascending=False).reset_index(drop=True)

    N = len(SortedData)
    GINI = abs(((N + 1) - 2 * np.sum((N - np.arange(1, N + 1) + 0.5) * SortedData) / np.sum(SortedData)) / N)
    return GINI


def LorenzCurve(Data):
    Data = np.array(Data)
    Data = np.sort(Data)
    n = len(Data)
    y = np.cumsum(Data) / np.sum(Data)
    x = np.linspace(0, 1, n)
    plt.figure(figsize=(10, 8))
    plt.plot(x, y, color='blue')
    plt.plot(x, x, linestyle='--', color='red')
    plt.xlabel('Cumulative Share of Population', labelpad=25)
    plt.ylabel('Cumulative Share of Income', labelpad=25)
    for i in range(0, 11):
        plt.axhline(y=i / 10, color='lightgray', linestyle='dotted', alpha=0.5)
        plt.axvline(x=i / 10, color='lightgray', linestyle='dotted', alpha=0.5)
    plt.legend(['Lorenz Curve', 'Equality Line', 'GINI Coefficient: {:.4f}'.format(CalculateGINI(pd.Series(Data)))], loc='upper left', frameon=False)
    plt.show()


def AtkinsonIndex(Data, Epsilon=0.5):
    SumATK = 0
    Count = 0
    for i in Data:
        SumATK += i ** (1 - Epsilon)
        Count += 1

    Mean = Data.mean()
    EDEATK = (SumATK / len(Data)) ** (1 / (1 - Epsilon))
    AtkinsonValue: float = 1 - EDEATK / Mean
    return AtkinsonValue


def TheilIndex(Data):
    Data = np.array(Data)
    L = sum([(x / np.sum(Data)) * np.log(len(Data) * x / np.sum(Data)) for x in Data if x != 0])
    return L
