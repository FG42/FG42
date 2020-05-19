.PHONY: update
update:
	@git remote set-url origin git://gitlab.com/FG42/FG42.git
	@git pull origin master

.PHONY: install-cli
install-ci:
	@echo "Downloading and installing fonts..."
	@mkdir -p ~/.fonts
	@wget "https://github.com/FG42/fonts/archive/0.1.0.tar.gz" -O ~/.fonts/fg42.tar.gz
	@tar zxf ~/.fonts/fg42.tar.gz -C ~/.fonts --strip 1
	@cp ./config/fg42.user.el ${HOME}/.fg42.el
	@echo "Creating the link..."
	@echo "#! /bin/sh" > ./fg42
	@echo "export FG42_HOME=$(shell pwd)" >> ./fg42
	@echo 'emacs --name FG42 --no-site-file --no-site-lisp --no-splash --title FG42 -l $$FG42_HOME/fg42-config.el "$$@"' >> ./fg42
	@chmod +x ./fg42

.PHONY: install
install:
	@echo "Downloading and installing fonts..."
	@mkdir -p ~/.fonts
	@wget "https://github.com/FG42/fonts/archive/0.1.0.tar.gz" -O ~/.fonts/fg42.tar.gz
	@tar zxf ~/.fonts/fg42.tar.gz -C ~/.fonts --strip 1
	@cp ./config/fg42.user.el ${HOME}/.fg42.el
	@echo "Creating the link..."
	@echo "#! /bin/sh" > ./fg42
	@cp ./scripts/fg42-wm ./fg42-wm
	@echo "export FG42_HOME=$(shell pwd)" >> ./fg42
	@echo "export FG42_HOME=$(shell pwd)" >> ./fg42-wm
	@echo 'FG42_WM=false emacs --name FG42 --no-site-file --no-site-lisp --no-splash --title FG42 -l $$FG42_HOME/fg42-config.el "$$@"' >> ./fg42
	@echo 'FG42_WM=true emacs --name FG42 --no-site-file --no-site-lisp --no-splash --title FG42 -l $$FG42_HOME/fg42-config.el "$$@"' >> ./fg42-wm
	@chmod +x ./fg42
	@chmod +x ./fg42-wm
	@sudo mkdir -p /usr/local/bin/
	@sudo rm -f /usr/local/bin/fg42
	@sudo rm -f /usr/local/bin/fg42-wm
	@sudo ln -s `pwd`/fg42 /usr/local/bin/fg42
	@sudo ln -s `pwd`/fg42-wm /usr/local/bin/fg42-wm
	@echo "Copying share files..."
	@sudo mkdir -p /usr/share/fg42/
	@sudo cp ./share/applications/fg42.desktop /usr/local/share/applications
	@sudo cp -r ./share/icons/hicolor/ /usr/local/share/icons
	@sudo cp -r ./share/* /usr/share/fg42/
	@sudo cp -r ./share/xsessions/fg42.desktop /usr/share/xsessions/
	@echo " "
	@echo "------------------------------------------------------------------------------------"
	@echo "Make sure to install external dependencies of FG42. For more info checkout README.md"
	@echo "Enjoy the bless of GNU/Emacs and FG42 :)"

.PHONY: install-fonts
install-fonts:
	@mkdir -p ~/.fonts/
	@cp -rv ./share/fonts/vazir/* ~/.fonts/

.PHONY: compile
compile:
	@$(PWD)/fg42-new --script scripts/compiler.el

.PHONY: clean
clean:
	@rm -rf $(shell find `pwd` -iname "*~")
	@rm -rf $(shell find `pwd`/core -iname "*.elc")
