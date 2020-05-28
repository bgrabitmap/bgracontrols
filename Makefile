ifeq ($(OS),Windows_NT)     # true for Windows_NT or later
  COPY := winmake\copyfile
  REMOVE := winmake\remove
  REMOVEDIR := winmake\removedir
  THEN := &
  RUN :=
else
  COPY := cp
  REMOVE := rm -f
  REMOVEDIR := rm -rf
  THEN := ;
  RUN := ./
  RUN := $(strip $(RUN))
endif

all: compile

install: not_installable
uninstall: not_installable

not_installable:
	echo "The library cannot be installed on the system but statically linked to another Lazarus package or application."

clean: clean_bgracontrols

clean_bgracontrols:
	$(REMOVEDIR) "lib"
	$(REMOVEDIR) "backup"

compile: BGRAControls
lazbuild:
	#lazbuild will determine what to recompile
BGRAControls: lazbuild bgracontrols.lpk
	lazbuild bgracontrols.lpk

