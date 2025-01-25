import csv
import warnings
from pathlib import Path
from sklearn.linear_model import LinearRegression, LogisticRegression


def _load_iris():
    with (Path(__file__).parent / 'iris.csv').open() as csv_file:
        datka = list(zip(*(map(float, row) for row in csv.reader(csv_file))))
    x = list(zip(*datka[:-1]))
    y = datka[-1]
    return x, y


def linear_reference():
    x, y = _load_iris()
    linear_regression = LinearRegression()
    linear_regression.fit(x, y)
    print(f'Linear regression reference weights: {linear_regression.coef_}')


def logistic_reference():
    x, y = _load_iris()
    logistic_regression = LogisticRegression()
    logistic_regression.fit(x, y)
    # will differ because scikit-learn's logreg is smort
    print(f'Logistic regression reference weights: {logistic_regression.coef_}')


if __name__ == '__main__':
    linear_reference()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        logistic_reference()
