# See instructions for installing pybind11

http://people.duke.edu/~ccc14/sta-663-2016/18G_C++_Python_pybind11.html


# Installing pybind11:

pip3 install pybind11

conda install -c conda-forge pybind11


Example 2: Compilation

- Note that since the header was not in the correct location, I needed to specify the location with the -I flag
- I am specifying the location of the anaconda installation

```
$ c++ -O3 -Wall -shared -I/home/sovereign/anaconda/include/python3.5m/ -std=c++11 -fPIC example.cpp -o example`python3-config --extension-suffix`
```


# Install Eigen
hg clone https://bitbucket.org/eigen/eigen/
