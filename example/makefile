### Konfiguration ###################################################

SRC = hello.bas
PRG = build/hello.prg
D81 = build/disk.d81
DISKNAME = MYPROG,01
# MEGA65_IP = 192.168.0.65

PETCAT = petcat
C1541 = c1541
FTP = mega65_ftp
M65 = m65

####################################################################

all: $(D81)

# BASIC → PRG
$(PRG): $(SRC)
	mkdir -p build
	$(PETCAT) -ic -w65 -o $(PRG) --  $(SRC)

# D81 erzeugen + PRG einfügen
$(D81): $(PRG)
	mkdir -p build
	$(C1541) -format $(DISKNAME) d81 $(D81)
	$(C1541) $(D81) -write $(PRG) autoboot.c65

####################################################################

# Deployment des D81-Images auf den echten MEGA65
deploy: $(D81)
	$(FTP) -e put $(D81)

# PRG direkt in den RAM (für schnelle Iterationen)
run: $(PRG)
	$(M65) -l $(PRG)

# Image inspizieren
list:
	$(C1541) $(D81) -list

clean:
	rm -rf build

