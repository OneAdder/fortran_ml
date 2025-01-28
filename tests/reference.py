import csv
import warnings
from pathlib import Path
import torch
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

def torch_linear():
    x = torch.tensor(torch.ones(3, 4), requires_grad=True)
    linear = torch.nn.Linear(in_features=4, out_features=2, bias=True)
    linear.weight.data = torch.zeros(2, 4) + 0.2
    linear.bias.data = torch.zeros(2) + 0.02
    y = linear(x)
    print(f'Forward on LinearLayer:\n{y}')
    y.backward(torch.zeros(3, 2) + 0.1)
    print(f'Gradient:\n{x.grad}')
    print(f'dw:\n{linear.weight.grad}')
    print(f'db:\n{linear.bias.grad}')


if __name__ == '__main__':
    linear_reference()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        logistic_reference()
    torch_linear()
