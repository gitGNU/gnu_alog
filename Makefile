all:
	@mkdir -p obj data
	@gnatmake -Palog
	@gnatmake -Palog_lib

clean:
	@rm -rf obj/*
	@rm -rf lib/*

distclean:
	@rm -rf obj

control:
	@rm -f obj/*.adt objects/*.ali
	cd obj && adactl -f ../rules/alog.aru ../*.ad[bs]

tests: all
	@obj/runner

