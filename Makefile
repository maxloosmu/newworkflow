all:
	ghc --make Main.hs -o transform

clean:
	rm *.o *.hi
