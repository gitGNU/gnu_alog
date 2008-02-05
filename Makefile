all:
	@mkdir -p obj data
	@gnatmake -Palog
clean:
	@rm -rf obj/*

distclean:
	@rm -rf obj
