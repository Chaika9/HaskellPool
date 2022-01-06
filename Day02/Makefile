SRC = 	Doop.hs

OBJ = $(SRC:.hs=.o)

all: doop
.PHONY : all

doop:
	@ghc -o $@ $(SRC)
.PHONY : doop

clean:
	@find . -name "*.hi" -delete
	@rm -f $(OBJ)
.PHONY : clean

fclean: clean
	@rm -f doop
.PHONY : fclean