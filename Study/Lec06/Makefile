OCAMLBUILD = ocamlbuild
LIBSB = unix,str,dynlink,nums
PROPERERRS = -warn-error,+4+8+9+11+12+25+28
FLAGS = $(PROPERERRS),-annot,-ccopt,-fopenmp 
OBB_FLAGS = -no-links -libs $(LIBSB) -cflags $(FLAGS) -lflags $(FLAGS) -lexflag -q -yaccflag -v  
 
all: byte

byte: 
	@ocamlbuild $(OBB_FLAGS) lec06.byte lec6_lecture_notes.byte
	cp -u _build/lec06.byte lec06.exe
	cp -u _build/lec6_lecture_notes.byte lec6_lecture_notes.exe


