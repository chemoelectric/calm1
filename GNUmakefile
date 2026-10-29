# Copyright © 2026 Barry Schwartz
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions: The above copyright notice and
# this permission notice shall be included in all copies or
# substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

.DELETE_ON_ERROR:
.DEFAULT_GOAL := default

VERSION = 1.0.0

SED = sed
CHICKEN_INSTALL = chicken-install
CHICKEN_UNINSTALL = chicken-uninstall

.PHONY: default all clean veryclean
default: all

all:: calm1

%: %.in
	$(SED) -e 's/@VERSION@/$(VERSION)/g' < $(<) > $(@).tmp
	mv $(@){.tmp,}

calm1: calm1.egg calm1-lib/version.sld
	$(CHICKEN_INSTALL) -n
veryclean:: ; -rm -f calm1.egg calm1-lib/version.sld

.PHONY: install
install:: calm1
	$(CHICKEN_INSTALL) -s

.PHONY: uninstall
uninstall::
	$(CHICKEN_UNINSTALL) -s -force calm1

clean::
	-rm -f calm1
	-rm -f calm1.build.sh
	-rm -f calm1.install.sh
	-rm -f calm1-lib.*.{o,so,link,import.scm}
