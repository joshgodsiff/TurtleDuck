all: Turtle

Turtle: Lexer Parser
	ghc --make -O2 -o Compiler Parser.hs

Lexer: Turtle.x
	alex Turtle.x -o Lexer.hs

Parser: Turtle.y Lexer
	happy Turtle.y -o Parser.hs

clean:
	rm Lexer.hs Parser.hs *.o *.hi Compiler
