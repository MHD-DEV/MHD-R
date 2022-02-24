FF = ifort
FFLAGS = -I.

OBJS =  Alloca_Vars.o  Julday.o   LeFix.o  Modelo.o   Parcunge.o    Simula.o \
    Caldat360.o       Gera_Medias.o    LeBacia.o      Musk.o     Sort.o \
	Caldat366.o    InterpMusk.o   LeCell.o    Musk_NL.o      Rede.o  \
	   Celula.o     Julday360.o    LeEsc.o    MHD.o     Parcel.o   RedeIni.o  \

DEPS = Vars_main.o

#%.mod: %.f90 $(DEPS)
#	$(FF) -c $@ $< $(FFLAGS)

%.o: %.f90 
	$(FF) -c -o $@ $< $(FFLAGS)

MHD: $(DEPS) $(OBJS)
	$(FF) -o $@ $(DEPS) $(OBJS) $(FFLAGS)

clean:
	rm -f *.o *~ core *~ *.mod 
