LIBS=-lvirt
GHC=ghc $(LIBS) --make
C2HS=c2hs

all: Test

Test: Test.hs System/LibVirt.hs System/LibVirt/Foreign.hs System/LibVirt/Errors.hs
	$(GHC) $<

%.hs: %.chs
	$(C2HS) $<

clean:
	rm -f *.o *.hi *.chi *.chs.h
	rm Test
	rm LibVirt.hs
