all:
	@mkdir -p obj data
	@gnatmake -Palog
clean:
	@rm -rf obj/*

distclean:
	@rm -rf obj

control:
	@rm -f obj/*.adt objects/*.ali
	cd obj && adactl -f ../rules/alog.aru ../*.ad[bs]

tests: all
	@obj/runner

