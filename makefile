# Makefile pour anagramme.ml

# Variables
OCAMLC = ocamlc
SOURCE = anagramme.ml
TARGET = anagramme

# Cible par défaut
all: $(TARGET)

# Compilation du programme
$(TARGET): $(SOURCE)
	$(OCAMLC) -o $(TARGET) $(SOURCE)

# Nettoyage des fichiers temporaires et de l'exécutable
clean:
	rm -f *.cmi *.cmo $(TARGET)

# Cible pour exécuter le programme
run: $(TARGET)
	./$(TARGET) $(arg1) $(arg2)

.PHONY: all clean run
