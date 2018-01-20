TARGET := `guile -c "(display (string-append (car %load-path) \"/ice-9\"))"`


all:
	@echo "Just type \"sudo make install\""

install:
	cp -fr ice-9/colorized.scm $(TARGET) 

uninstall:
	rm -r $(TARGET)/colorized.scm 
