LIBS=-lvirt
GHC=ghc $(LIBS) --make
C2HS=c2hs

all: Test

Test: Test.hs LibVirt.hs
	$(GHC) $<

%.hs: %.chs
	$(C2HS) $<

clean:
	rm -f *.o *.hi *.chi *.chs.h
	rm Test
	rm LibVirt.hs
