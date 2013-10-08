all: Parser


Lexer: Turtle.x
	alex Turtle.x -o Lexer.hs

Parser: Turtle.y Lexer
	happy Turtle.y -o Parser.hs

clean:
	rm Lexer.hs Parser.hs
