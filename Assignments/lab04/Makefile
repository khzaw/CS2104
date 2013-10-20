OCAMLBUILD = ocamlbuild
LIBSB = unix,str,dynlink,nums
PROPERERRS = -warn-error,+4+8+9+11+12+25+28
FLAGS = $(PROPERERRS),-annot,-ccopt,-fopenmp 
OBB_FLAGS = -no-links -libs $(LIBSB) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  
 all: byte
byte: 
	@ocamlbuild $(OBB_FLAGS) lab4.byte
	cp -u _build/lab4.byte lab4
