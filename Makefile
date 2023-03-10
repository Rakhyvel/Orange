run:
	gcc Orangec/*.c util/*.c -Wall -o orangec
	./orangec test/*.orng test/ornglib/*.orng -o test/test.js -t web

verbose:
	gcc Orangec/*.c util/*.c -Wall -o orangec -DVERBOSE
	./orangec test/*.orng test/ornglib/*.orng -o test/test.js -t web

git-commit:
	git add .
	git commit -m "$(msg)"

git-push:
	git push origin master

clean:
	rm orangec