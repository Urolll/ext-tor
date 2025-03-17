OCAMLC = ocamlc
OCAMLFLAGS = -o module

SRC = module.ml
OUT = module
CMI = module.cmi
CMO = module.cmo

all: $(OUT)
	./$(OUT)

$(OUT): $(SRC)
	$(OCAMLC) $(OCAMLFLAGS) $(SRC)

clean:
	rm -f $(OUT) $(CMO) $(CMI)
