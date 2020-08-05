
default:
	cd core && make
	cd app && make fresh

docs: default
	cd core && make docs

clean:
	cd core && make clean
	cd app && make clean
