all:
	swig -cffi -c++  small.i
	g++ -c small_wrap.cxx -fPIC -o small_wrap.o
	g++ -c small.cpp      -fPIC -o small.o
	g++ -shared small.o small_wrap.o -o small.so

test:
	ccl < small_test.cl > small_test.output_observed
	echo "if the tests passed, there should be a message here:"
	tail small_test.output_observed

