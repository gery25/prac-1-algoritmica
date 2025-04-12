
practica=illa

$(practica): $(practica).hs
	ghc $<

instances: 
	python3 Generator.py 4 12 

test: $(practica).py $(practica)
	python3 Checker.py $(practica).py 4 12
	python3 Checker.py ./$(practica) 4 12

zip:
	$(RM) scripts.zip
	zip -r scripts.zip Makefile Checker.py $(practica).py $(practica).hs Generator.py

clean:
	$(RM) $(practica)

