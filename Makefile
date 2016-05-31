mbt.html: mbt.org
	emacs --batch -l ~/.emacs.d/init.el \
		--visit "$<" \
		--funcall org-html-export-to-html

clean:
	rm -f *~
	rm -f mbt.html
