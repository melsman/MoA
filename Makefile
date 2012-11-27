
MLKITLIB=$(HOME)/mlkit-4.3.6
#MLCOMP=SML_LIB=$(MLKITLIB)/lib/mlkit $(MLKITLIB)/bin/mlkit
MLCOMP=mlton

UTEST_FILES=utest/utest.sig utest/utest.sml utest/utest.mlb
VEC_FILES=vec/vec.sig vec/fvec.sml vec/list_vec.sml vec/ppvec.sml vec/vec.mlb vec/test_vec.sml
IL_FILES=ilvec/il.mlb ilvec/il.sml ilvec/ilutil.sig ilvec/ilutil.sml ilvec/test_il.sml ilvec/test_ilvec.sml ilvec/ilvec.sig ilvec/ilvec.sml
IL2_FILES=ilvec/il2.mlb ilvec/il2.sml ilvec/ilutil2.sig ilvec/ilutil2.sml ilvec/ilvec2.sig ilvec/ilvec2.sml ilvec/test_il.sml ilvec/test_ilvec2.sml
MOA_FILES=moa.sig moa.sml test_moa.sml moa.mlb
ILMOA_FILES=ilmoa.sig ilmoa.sml test_ilmoa.sml
FILES=Makefile $(IL2_FILES) $(IL_FILES) $(UTEST_FILES) $(VEC_FILES) $(MOA_FILES) $(ILMOA_FILES)

all: moa runvec runil runil2 ilmoa

moa: $(FILES)
	$(MLCOMP) -output $@ moa.mlb

ilmoa: $(FILES)
	$(MLCOMP) -output $@ ilmoa.mlb

runilmoa: $(FILES)
	$(MLCOMP) -output $@ testilmoa.mlb

runvec: $(FILES)
	$(MLCOMP) -output $@ vec/vec.mlb

runil: $(FILES)
	$(MLCOMP) -output $@ ilvec/il.mlb

runil2: $(FILES)
	$(MLCOMP) -output $@ ilvec/testil2.mlb

clean:
	find . -name 'MLB' | xargs rm -rf
	find . -name '*~' | xargs rm -f
	rm -f moa run runvec runil runil2 ilmoa
