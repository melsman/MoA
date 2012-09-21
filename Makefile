
MLKITLIB=$(HOME)/mlkit-4.3.6
#MLCOMP=SML_LIB=$(MLKITLIB)/lib/mlkit $(MLKITLIB)/bin/mlkit
MLCOMP=mlton

UTEST_FILES=utest/utest.sig utest/utest.sml utest/utest.mlb
VEC_FILES=vec/vec.sig vec/fvec.sml vec/list_vec.sml vec/ppvec.sml vec/vec.mlb vec/test_vec.sml
IL_FILES=ilvec/il.mlb ilvec/il.sml ilvec/ilutil.sig ilvec/ilutil.sml ilvec/test_il.sml ilvec/test_ilvec.sml ilvec/ilvec.sig ilvec/ilvec.sml
FILES=Makefile $(IL_FILES) $(UTEST_FILES) $(VEC_FILES) moa.sig moa.sml test_moa.sml moa.mlb

.PHONY: moa
all: moa runvec runil

moa: $(FILES)
	$(MLCOMP) -output $@ moa.mlb

runvec: $(FILES)
	$(MLCOMP) -output $@ vec/vec.mlb

runil: $(FILES)
	$(MLCOMP) -output $@ ilvec/il.mlb

clean:
	find . -name 'MLB' | xargs rm -rf
	find . -name '*~' | xargs rm -f
	rm -f moa run runvec runil
