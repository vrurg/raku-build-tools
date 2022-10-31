
.NOTPARALLEL:

SHELL = /bin/sh

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
BUILD_TOOLD_DIR?=./build-tools
META_BUILDER?=$(BUILD_TOOLD_DIR)/gen-META.raku
DOC_BUILDER?=$(BUILD_TOOLD_DIR)/gen-doc.raku
DOC_BUILD_ARGS?=--module=$(MAIN_MOD)

TEST_JOBS?=4
PROVE_CMD?=prove6
PROVE_FLAGS?=-I. -j$(TEST_JOBS)
TEST_DIRS?=t
PROVE?=$(PROVE_CMD) $(PROVE_FLAGS) $(TEST_DIRS)

DIST_FILES:=$(shell git ls-files -z)

CLEAN_FILES?=$(MOD_NAME_PFX)-v*.tar.gz \
			META6.json.out

DEPENDECIES=fez Async::Workers Pod::To::Markdown App::Prove6

PRECOMP_DIRS?=$(shell find . -type d -name '.precomp')
BK_FILES?=$(shell find . -name '*.bk')
CLEAN_DIRS?=$(PRECOMP_DIRS) $(BK_FILES) .test-repo

# Doc variables
DOC_DIR?=doc
DOCS_DIR?=docs
MD_DIR?=$(DOCS_DIR)/md
HTML_DIR?=$(DOCS_DIR)/html
DOCS_SUBDIRS?=$(shell find lib -type d -name '.*' -prune -o -type d -printf '%P\n')
MD_SUBDIRS:=$(addprefix $(MD_DIR)/,$(DOCS_SUBDIRS))
HTML_SUBDIRS:=$(addprefix $(HTML_DIR)/,$(DOCS_SUBDIRS))
PM_SRC?=$(shell git ls-files lib | xargs grep -l '^=begin')
POD_SRC?=$(shell git ls-files $(DOC_DIR) | grep -vi 'readme.rakudoc' | grep -vi 'changelog.rakudoc')
DOC_SRC?=$(POD_SRC) $(PM_SRC)
DOC_DEST?=$(shell git ls-files lib $(DOC_DIR) | xargs grep -l '^=begin' | sed 's,^[^/]*/,,')
README_SRC?=$(MAIN_MOD_FILE)
CHANGELOG_SRC?=$(DOC_DIR)/ChangeLog.rakudoc

export MAIN_MOD META_MOD RAKU_OPTS

.SUFFXES: .md .rakudoc

vpath %.rakumod $(dir $(PM_SRC))
vpath %.rakudoc $(dir $(POD_SRC))

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

#docs_dirs: | $(MD_SUBDIRS) $(HTML_SUBDIRS)

docs_dirs: | $(MD_SUBDIRS)

$(MD_SUBDIRS) $(HTML_SUBDIRS):
	$(NOECHO)echo "===> mkdir" $@
	$(NOECHO)mkdir -p $@

doc_gen: $(DOC_BUILDER)
	$(NOECHO)echo "===> Updating documentation sources"
	$(NOECHO)raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) -md $(DOC_SRC)
	$(NOECHO)raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) --md --output=./README.md $(README_SRC)
	$(NOECHO)if [ -e $(CHANGELOG_SRC) ]; then raku $(RAKU_OPTS) $(DOC_BUILDER) $(DOC_BUILD_ARGS) --md --output=./ChangeLog.md $(CHANGELOG_SRC); fi

#md: ./README.md $(addprefix $(MD_DIR)/,$(patsubst %.rakudoc,%.md,$(patsubst %.rakumod,%.md,$(DOC_DEST))))

#html: $(addprefix $(HTML_DIR)/,$(patsubst %.rakudoc,%.html,$(patsubst %.rakumod,%.html,$(DOC_DEST))))

test:
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
	$(NOECHO)/bin/sh -c 'for dep in $(DEPENDECIES); do (zef locate $$dep 2>&1 >/dev/null || (echo "++++ Installing $$dep"; zef install $$dep)); done'
	$(NOECHO)zef --deps-only install .

version: doc meta clean
#	@git add . && git commit -m 'Minor: version bump'

release: build is-repo-clean release-test archive
	$(NOECHO)echo "===> Done releasing"

meta6_mod:
	$(NOECHO)zef locate META6 2>&1 >/dev/null || (echo "===> Installing META6"; zef install META6)

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
	$(NOECHO)fez upload
	$(NOECHO)echo "===> Uploaded!"

clean:
	$(NOECHO)echo "===> Cleaning " $(CLEAN_FILES) $(CLEAN_DIRS)
	$(NOECHO)rm -f $(CLEAN_FILES)
	$(NOECHO)rm -rf $(CLEAN_DIRS)

install: build
	$(NOECHO)echo "===> Installing"
	$(NOECHO)zef install .

# vim: ft=make
