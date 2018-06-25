import wrap
import numpy as np

def test_add():
    assert(wrap.add(3, 4) == 7)
    print('wrapfunc: ', wrap.add(3,2))

if __name__ == '__main__':
	test_add()
	
	# Test numpy functionality
	print('\n')
	xs = np.random.rand(10)
	print(xs)
	print(wrap.square(xs))
	
	# Numpy Test 
	print("np :", xs.sum())
	print("cpp:", wrap.sum(xs))
	
	# Twice:
	print(xs)
	
	# Operates on the array
	wrap.twice(xs)
	print("twice:", xs)
	
    
    
