
EXE=_build/default/main.exe

all: $(EXE) rapport

$(EXE): *.ml*
	dune build @all
	cp $(EXE) ngoc 

test: $(EXE) test.go
	./ngoc --debug test.go
	gcc -g -no-pie test.s -o test
	./test
	go run test.go

rapport:
	(pdflatex rapport.tex -o rapport.pdf) > rapportLog.txt

export-%:
	cp test.go ../tests/exec/$*.go
	go run test.go > ../tests/exec/$*.out

.PHONY: clean
clean:
	dune clean
	rm ngoc *.dot *.png *.aux *.log *.toc *.pdf *.txt tst/good/*.s tst/good/*.out tst/bad_typing/*.s tst/bad_typing/*.out tst/bad_compile/*.s tst/bad_compile/*.out 

