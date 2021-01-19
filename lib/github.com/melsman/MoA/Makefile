
MLCOMP ?= mlton

UTEST_FILES=utest/utest.sig utest/utest.sml utest/utest.mlb
VEC_FILES=vec/vec.sig vec/fvec.sml vec/list_vec.sml vec/ppvec.sml vec/vec.mlb vec/test_vec.sml
IL_FILES=ilvec/il.mlb ilvec/ilm.mlb ilvec/il.sml ilvec/ilutil.sig ilvec/ilutil.sml ilvec/test_il.sml ilvec/test_ilvec.sml ilvec/ilvec.sig ilvec/ilvec.sml ilvec/test_ilvecm.sml ilvec/ilvecm.sig ilvec/ilvecm.sml
IL2_FILES=ilvec/il2.mlb ilvec/il2.sml ilvec/ilutil2.sig ilvec/ilutil2.sml ilvec/ilvec2.sig ilvec/ilvec2.sml ilvec/test_il.sml ilvec/test_ilvec2.sml
MOA_FILES=moa.sig moa.sml test_moa.sml moa.mlb
ILMOA_FILES=ilmoa.sig ilmoa.sml test_ilmoa.sml
ILAPL_FILES=ilapl.sig ilapl.sml test_ilapl.sml mla.sml ilvec/il2m.mlb ilvec/ilvec2m.sml ilvec/ilvec2m.sig testilapl.mlb
FILES=Makefile $(IL2_FILES) $(IL_FILES) $(UTEST_FILES) $(VEC_FILES) $(MOA_FILES) $(ILMOA_FILES) $(ILAPL_FILES)

all: moa.exe runvec.exe runil.exe runil2.exe ilmoa.exe runilm.exe runilmoa.exe
#runilapl.exe
#runil2m.exe


test: all
	./moa.exe
	./runvec.exe
	./runil.exe
	./runil2.exe
	./runilmoa.exe
	./runilm.exe
#	./runil2m.exe
#	./runilapl.exe

moa.exe: $(FILES)
	$(MLCOMP) -output $@ moa.mlb

ilmoa.exe: $(FILES)
	$(MLCOMP) -output $@ ilmoa.mlb

ilapl.exe: $(FILES)
	$(MLCOMP) -output $@ ilapl.mlb

runilmoa.exe: $(FILES)
	$(MLCOMP) -output $@ testilmoa.mlb

runilapl.exe: $(FILES)
	$(MLCOMP) -output $@ testilapl.mlb

runvec.exe: $(FILES)
	$(MLCOMP) -output $@ vec/vec.mlb

runil.exe: $(FILES)
	$(MLCOMP) -output $@ ilvec/il.mlb

runilm.exe: $(FILES)
	$(MLCOMP) -output $@ ilvec/ilm.mlb

runil2m.exe: $(FILES)
	$(MLCOMP) -output $@ ilvec/testil2m.mlb

runil2.exe: $(FILES)
	$(MLCOMP) -output $@ ilvec/testil2.mlb

clean:
	find . -name 'MLB' | xargs rm -rf
	find . -name '*~' | xargs rm -f
	rm -f moa.exe run runvec.exe runil.exe runil2.exe ilmoa.exe runilmoa.exe ilapl.exe runilapl.exe runilm.exe
