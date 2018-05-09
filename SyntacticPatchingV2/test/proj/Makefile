SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)
CC=gcc

all: $(OBJS)

%.o : %.c
	$(CC) -c $< -o $@
clean:
	rm -rf *.o *.ss *.he
