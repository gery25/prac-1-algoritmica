
practica=illa

$(practica): $(practica).hs
	ghc $<

instances: 
	python3 Generator.py 4 13

test: $(practica).py $(practica)
	python3 Checker.py $(practica).py 4 13
	python3 Checker.py ./$(practica) 4 13

zip:
	$(RM) scripts.zip
	zip -r scripts.zip Makefile Checker.py $(practica).py $(practica).hs Generator.py README.md

clean:
	$(RM) $(practica)

