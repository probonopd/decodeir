NAME=DecodeIR

all:	decodeir

clean:
	rm decodeir

decodeir:	$(NAME).cpp 
	$(CXX) -DMAIN -o $@ $(INCLUDES) $<
