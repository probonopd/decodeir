NAME=DecodeIR

all:	decodeir

clean:
	rm decodeir

decodeir:	$(NAME).cpp 
	g++ -DMAIN -o $@ $(INCLUDES) $<
