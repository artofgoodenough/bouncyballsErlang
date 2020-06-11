.PHONY: all clean

CC = g++
CXXFLAGS = -c -std=c++11 -Wall
LIBLOCATION=/usr
INCLUDES = -I$(LIBLOCATION)/include
LIBS = -lm -lpthread -L$(LIBLOCATION)/lib -lSDL2 -lSDL2_image

EXEC := dispBalls.exe
SOURCES := $(wildcard *.cpp)
HEADERS := $(wildcard *.h)
OBJECTS := $(SOURCES:.cpp=.o)
OBJFILES := $(addprefix obj/, $(OBJECTS))

$(EXEC): $(OBJECTS)
	$(CC) $(OBJFILES) $(LIBS) -o bin/$(EXEC)

%.o: %.cpp $(HEADERS)
	$(CC) $(CXXFLAGS) -I . $(INCLUDES) $< -o obj/$@

clean:
	rm -f bin/$(EXEC) $(OBJFILES)
