import os, sys

from distutils.core import setup, Extension
from distutils import sysconfig

cpp_args = ["'python3-config --extension-suffix'"]

ext_modules = [
    Extension(
    'example',
        ['example.cpp'],
        #include_dirs=['pybind11/include','/home/sovereign/anaconda/include/python3.5m/'],
        include_dirs=['/home/sovereign/anaconda/include/python3.5m/pybind11'],
    language='c++',
    libraries=['gsl', 'gslcblas'],
    #extra_compile_args = cpp_args,
    ),
]

setup(
    name='example',
    version='0.0.1',
    author='Cliburn Chan',
    author_email='cliburn.chan@duke.edu',
    description='Example',
    ext_modules=ext_modules,
)
