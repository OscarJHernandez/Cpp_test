import example
import numpy as np


if __name__ == '__main__':
    A = np.array([[1,2,1],
                  [2,1,0],
                  [-1,1,2]])

    print(A)
    print(example.det(A))
    print(example.inv(A))


