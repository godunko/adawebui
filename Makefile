
all:
	gprbuild -p -P gnat/adawebui.gpr

clean:
	rm -rf .objs
