
MLKITLIB=$(HOME)/mlkit-4.3.6
#MLCOMP=SML_LIB=$(MLKITLIB)/lib/mlkit $(MLKITLIB)/bin/mlkit
MLCOMP=mlton

UTEST_FILES=utest/utest.sig utest/utest.sml utest/utest.mlb
VEC_FILES=vec/vec.sig vec/fvec.sml vec/list_vec.sml vec/vec.mlb
FILES=$(UTEST_FILES) $(VEC_FILES) moa.sig moa.sml test_moa.sml moa.mlb

.PHONY: moa
all: moa runvec

moa: $(FILES)
	$(MLCOMP) -output $@ moa.mlb

runvec: $(FILES)
	$(MLCOMP) -output $@ vec/vec.mlb

clean:
	find . -name 'MLB' | xargs rm -rf
	find . -name '*~' | xargs rm -f
	rm -f moa run runvec
