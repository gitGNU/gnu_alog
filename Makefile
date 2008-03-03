all:
	@mkdir -p data lib obj
	@gnatmake -Palog
	@gnatmake -Palog_lib

clean:
	@rm -rf obj/*
	@rm -rf lib/*

distclean:
	@rm -rf obj
	@rm -rf lib

control:
	@rm -f obj/*.adt objects/*.ali
	cd obj && adactl -f ../rules/alog.aru ../src/*.ad[bs]

tests: all
	@obj/runner

