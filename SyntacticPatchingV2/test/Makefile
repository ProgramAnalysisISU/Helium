SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

all: $(OBJS)

%.o : %.c
	hc -c $< -o $@
clean:
	rm -rf *.o *.scm
