all:
	#swig -c++ -cffi -debug-module 4 testlib.swig
	swig -c++ -cffi  testlib.swig
	g++ -c testlib_wrap.cxx -fPIC -o testlib_wrap.o
	g++ -c testlib.cpp      -fPIC -o testlib.o
	g++ -shared testlib.o testlib_wrap.o -o testlib.so

clean:
	rm -f *.o testlib_wrap.cxx testlib-clos.lisp testlib.lisp testlib.so *.lx64fsl

stop:
	make 2>&1 > full.log ; cat full.log |grep stopping -A 2 > stopping.log

s:
	cd ~/dj/swig; ./remake

small:
	cd ~/dj/swig; ./remake
	#swig -cffi -c++ -debug-module 4 small.i 
	swig -cffi -c++  small.i
	g++ -c small_wrap.cxx -fPIC -o small_wrap.o
	g++ -c small.cpp      -fPIC -o small.o
	g++ -shared small.o small_wrap.o -o small.so

test:
	ccl < small_test.cl > small_test.output_observed
	echo "if the tests passed, there should be a message here:"
	tail small_test.output_observed

