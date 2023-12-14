
.NOTPARALLEL:

SHELL = /bin/sh

ifndef BASE_DIR
BASE_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))/..))
endif

NOECHO?=@

RAKU_OPTS=-I.

MAIN_MOD_FILE=$(addprefix lib/,$(addsuffix .rakumod,$(subst ::,/,$(MAIN_MOD))))
MOD_VER:=$(shell raku $(RAKU_OPTS) -e 'use $(MAIN_MOD); $(MAIN_MOD).^ver.say')
MOD_NAME_PFX?=$(subst ::,-,$(MAIN_MOD))
MOD_DISTRO?=$(MOD_NAME_PFX)-$(MOD_VER)
MOD_ARCH?=$(MOD_DISTRO).tar.gz
META?=META6.json
META_MOD?=$(MAIN_MOD)::META
META_MOD_FILE?=$(addprefix lib/,$(addsuffix .rakumod,$(subst ::,/,$(META_MOD))))
BUILD_TOOLS_DIR?=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))
META_BUILDER?=$(BUILD_TOOLS_DIR)/gen-META.raku
DOC_BUILDER?=$(BUILD_TOOLS_DIR)/gen-doc.raku
DOC_BUILD_ARGS?=--module=$(MAIN_MOD)

TEST_JOBS?=$(shell raku -e 'say $*KERNEL.cpu-cores')

ifeq "$(OS)" "Windows_NT"
OSNAME:=win
else
OSNAME:=$(shell uname)
endif

ifeq "$(OSNAME)" "Darwin"
DYLD_ENV?=DYLD_LIBRARY_PATH=/opt/local/lib
else
DYLD_ENV?=
endif

PROVE6:=$(shell which prove6)
PROVE5:=$(shell which prove)
ifneq "$(PROVE6)" ""
	PROVE_CMD?=raku $(PROVE6)
	PROVE_FLAGS?=-I. -j$(TEST_JOBS)
else ifneq "$(PROVE5)" ""
	PROVE_CMD?=$(PROVE5) -e "raku -I."
	PROVE_FLAGS?=--ext "rakutest" -j$(TEST_JOBS)
else
	$(error Neither prove6 nor prove utility is found)
endif
TEST_DIRS?=t
PROVE?=$(DYLD_ENV) $(PROVE_CMD) $(PROVE_FLAGS) $(TEST_DIRS)

ZEF:=$(DYLD_ENV) raku $(shell which zef)

DIST_FILES:=$(shell git ls-files -z)

CLEAN_FILES?=$(MOD_NAME_PFX)-v*.tar.gz \
			META6.json.out

DEPENDECIES = fez Async::Workers Pod::To::Markdown App::Prove6 URI LibXML \
		      Digest::SHA256::Native OO::Monitors Template::Mustache JSON::Fast \
			  Cmark

PRECOMP_DIRS?=$(shell find . -type d -name '.precomp')
BK_FILES?=$(shell find . -name '*.bk')
CLEAN_DIRS?=$(PRECOMP_DIRS) $(BK_FILES) .test-repo

# Doc variables
# !!! DOC_DIR is DEPRECATED!
DOC_DIR?=doc
# Use this for transition time compatibility
DOC_SRC_DIR?=$(DOC_DIR)
DOCS_DIR?=docs
MD_DIR?=$(DOCS_DIR)/md
HTML_DIR?=$(DOCS_DIR)/html
DOCS_SUBDIRS?=$(shell find lib -type d -name '.*' -prune -o -type d -printf '%P\n')
MD_SUBDIRS:=$(addprefix $(MD_DIR)/,$(DOCS_SUBDIRS))
HTML_SUBDIRS:=$(addprefix $(HTML_DIR)/,$(DOCS_SUBDIRS))
PM_SRC?=$(shell git ls-files lib | xargs grep -l '^=begin')
RAKUDOC_EXCLUDE ?= $(shell for d in "$(BASE_DIR)" "$(BUILD_TOOLS_DIR)"; do if [ -e "$$d/.rakudoc-exclude" ]; then echo "$$d/.rakudoc-exclude"; break; fi; done)
RAKUDOC_SRC?=$(shell git ls-files $(DOC_SRC_DIR) | grep -vif $(RAKUDOC_EXCLUDE) | sort)
DOC_SRC?=$(RAKUDOC_SRC) $(PM_SRC)
DOC_DEST?=$(shell git ls-files lib $(DOC_SRC_DIR) | xargs grep -l '^=begin' | sed 's,^[^/]*/,,')
README_SRC?=$(MAIN_MOD_FILE)
ifneq ("","$(wildcard $(DOC_SRC_DIR)/ChangeLog.rakudoc)")
	CHANGELOG_SRC?=$(DOC_SRC_DIR)/ChangeLog.rakudoc
endif

export MAIN_MOD META_MOD RAKU_OPTS DOCS_DIR DOC_DIR

.SUFFXES: .md .rakudoc

vpath %.rakumod $(dir $(PM_SRC))
vpath %.rakudoc $(dir $(RAKUDOC_SRC))

.PHONY: all html test author-test release-test is-repo-clean build depends depends-install release meta6_mod meta \
		archive upload clean install doc md html docs_dirs doc_gen version

tell_var:
	$(NOECHO)echo $(MOD_VER)

#%.md $(addsuffix /%.md,$(MD_SUBDIRS)):: %.rakumod
#	@echo "===> Generating" $@ "of" $<
#	@raku -I lib --doc=Markdown $< >$@
#
#%.md $(addsuffix /%.md,$(MD_SUBDIRS)):: %.rakudoc
#	@echo "===> Generating" $@ "of" $<
#	@raku -I lib --doc=Markdown $< >$@
#
#%.html $(addsuffix /%.html,$(HTML_SUBDIRS)):: %.rakumod
#	@echo "===> Generating" $@ "of" $<
#	@raku -I lib --doc=HTML $< >$@
#
#%.html $(addsuffix /%.html,$(HTML_SUBDIRS)):: %.rakudoc
#	@echo "===> Generating" $@ "of" $<
#	@raku -I lib --doc=HTML $< >$@

all: release

$(DOC_BUILDER) $(META_BUILDER):
	$(NOECHO)echo "===> Prepare submodule"
	$(NOECHO)git submodule sync --quiet --recursive
	$(NOECHO)git submodule init --quiet
	$(NOECHO)git submodule update --quiet --recursive

doc: docs_dirs doc_gen

docs_dirs: | $(MD_SUBDIRS) $(HTML_SUBDIRS)

# docs_dirs: | $(MD_SUBDIRS)

$(MD_SUBDIRS) $(HTML_SUBDIRS):
	$(NOECHO)echo "===> mkdir" $@
	$(NOECHO)mkdir -p $@

doc_gen: $(DOC_BUILDER)
	$(NOECHO)echo "===> Updating documentation sources"
	$(NOECHO)raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) --md --html $(DOC_SRC)
	$(NOECHO)raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) --md --html --title='README' --output=./README $(README_SRC)
ifdef CHANGELOG_SRC
	$(NOECHO)raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) --md --html --output=./ChangeLog $(CHANGELOG_SRC)
endif
	$(NOECHO)raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) --md --html --index --index-output=./INDEX
	$(NOECHO)raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) --md --html --check-links

#md: ./README.md $(addprefix $(MD_DIR)/,$(patsubst %.rakudoc,%.md,$(patsubst %.rakumod,%.md,$(DOC_DEST))))

#html: $(addprefix $(HTML_DIR)/,$(patsubst %.rakudoc,%.html,$(patsubst %.rakumod,%.html,$(DOC_DEST))))

test::
	$(NOECHO)echo "===> Testing"
	$(NOECHO)$(PROVE)

author-test:
	$(NOECHO)echo "===> Author testing"
	$(NOECHO)AUTHOR_TESTING=1 $(PROVE)

release-test:
	$(NOECHO)echo "===> Release testing"
	$(NOECHO)RELEASE_TESTING=1 $(PROVE)

is-repo-clean:
	$(NOECHO)git diff-index --quiet HEAD || (echo "*ERROR* Repository is not clean, commit your changes first!"; exit 1)

build: depends build-local doc

# Target to be overriden by module's custom Makefile
build-local:

depends: meta depends-install

depends-install:
	$(NOECHO)echo "===> Installing dependencies"
	$(NOECHO)/bin/sh -c 'for dep in $(DEPENDECIES); do ($(ZEF) locate $$dep 2>&1 >/dev/null || (echo "++++ Installing $$dep"; $(ZEF) install $$dep)); done'
	$(NOECHO)$(ZEF) install --deps-only --serial .

version: doc meta clean
#	@git add . && git commit -m 'Minor: version bump'

release: build is-repo-clean release-test archive
	$(NOECHO)echo "===> Done releasing"

meta6_mod:
	$(NOECHO)$(ZEF) locate META6 2>&1 >/dev/null || (echo "===> Installing META6"; $(ZEF) install META6)

archive: $(MOD_ARCH)

$(MOD_ARCH): $(DIST_FILES)
	$(NOECHO)echo "===> Creating release archive" $(MOD_ARCH)
	$(NOECHO)echo "Generating release archive will tag the HEAD with current module version."
	$(NOECHO)echo "Consider carefully if this is really what you want!"
	$(NOECHO)/bin/sh -c 'read -p "Do you really want to tag? (y/N) " answer; [ $$answer = "Y" -o $$answer = "y" ]'
	$(NOECHO)git tag -f $(MOD_VER) HEAD
	$(NOECHO)git push -f --tags
	$(NOECHO)git archive --prefix="$(MOD_DISTRO)/" -o $(MOD_ARCH) $(MOD_VER)

ifdef NO_META6

meta:

else

meta: meta6_mod $(META_BUILDER) $(META)

$(META): $(META_BUILDER) $(MAIN_MOD_FILE) $(META_MOD_FILE)
	$(NOECHO)echo "===> Generating $(META)"
	$(NOECHO)$(META_BUILDER) $(MAIN_MOD) >$(META).out && cp $(META).out $(META)
	$(NOECHO)rm $(META).out

endif

upload: release
	$(NOECHO)echo "===> UPLOADING..."
	$(NOECHO)/bin/sh -c 'read -p "Do you really want to upload to the ecosystem? (y/N) " answer; [ $$answer = "Y" -o $$answer = "y" ]'
	$(NOECHO)fez upload --file=$(MOD_ARCH)
	$(NOECHO)echo "===> Uploaded!"

clean:
	$(NOECHO)echo "===> Cleaning " $(CLEAN_FILES) $(CLEAN_DIRS)
	$(NOECHO)rm -f $(CLEAN_FILES)
	$(NOECHO)rm -rf $(CLEAN_DIRS)

install: build
	$(NOECHO)echo "===> Installing"
	$(NOECHO)$(ZEF) install .

# vim: ft=make
