LIBS=-lvirt
GHC=ghc $(LIBS) --make
C2HS=c2hs

all: Test

Test: Test.hs System/LibVirt.hs System/LibVirt/Internal.hs System/LibVirt/Foreign.hs System/LibVirt/Errors.hs
	$(GHC) $<

%.hs: %.chs
	$(C2HS) $<

clean:
	find . -name \*.o -delete
	find . -name \*.hi -delete
	find . -name \*.chi -delete
	find . -name \*.chs.h -delete
	rm Test
