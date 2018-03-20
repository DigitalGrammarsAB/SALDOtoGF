all: saldo2gf

saldo2gf: *.hs
	ghc -O2 -o saldo2gf Main.hs

clean:
	rm -f generate/*.gf* *.o *.hi *.pgf
