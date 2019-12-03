import sys
sys.path.insert(0, "../..")

if sys.version_info[0] >= 3:
	raw_input = input

import ply.lex as lex
import ply.yacc as yacc
import os

class Parser:
	"""
	Base class for a lexer/parser that has the rules defined as methods
	"""
	tokens = ()
	precedence = ()

	def __init__(self, **kw):
		self.debug = kw.get('debug', 0)
		self.names = {}
		self.degrees = [0, 0, 0]
		self.error = 0
		try:
			modname = os.path.split(os.path.splitext(__file__)[0])[
				1] + "_" + self.__class__.__name__
		except:
			modname = "parser" + "_" + self.__class__.__name__
		self.debugfile = modname + ".dbg"
		self.tabmodule = modname + "_" + "parsetab"
		# print self.debugfile, self.tabmodule

		# Build the lexer and parser
		lex.lex(module=self, debug=self.debug)
		yacc.yacc(module=self,
					debug=self.debug,
					debugfile=self.debugfile,
					tabmodule=self.tabmodule)

	def first_degree(self):
		if (self.c == 0):
			print("There is only one solution: X^1 = 0")
		else:
			print("result is {}".format(-self.c / self.b))
	
	def second_degree(self, a, b, c):
		d = float((b * b) - 4 * a * c)
		print("The discriminant is : {}".format(d))
		if d > 0:
			s1 = float((-b + d**0.5)/(2 * a))
			s2 = float((-b - d**0.5)/(2 * a))
			print("Discriminant is strictly positive, the two solutions are:\n{}\n{}".format(s1, s2))
		elif d == 0:
			s0 = float(-b / (2 * a))
			print("Discriminant is equal to 0, the solution is: {}".format(s0))
		elif d < 0:
			print("Discriminant is negative, there is no solution")

	def run(self):
		while 1:
			self.signs = [0, 0, 0]
			try:
				s = raw_input('calc > ')
			except EOFError:
				break
			if not s:
				print("0")
				continue
			if ("=" not in s):
				print("Don't fuck with me, insert an equal in that equation")
			else:
				first, second = s.split('=')
				yacc.parse(first)
				self.df = self.degrees
				self.degrees = [0, 0, 0]
				yacc.parse(second)
				if self.error == 1:
					print("Error in syntax")
					self.degrees = [0, 0, 0]
					self.error = 0
				else:
					self.ds= self.degrees
					self.a = self.df[2] - self.ds[2]
					self.b = self.df[1] - self.ds[1]
					self.c = self.df[0] - self.ds[0]
					self.degrees = [0, 0, 0]
					if (self.a == 0 and self.b == 0 and self.c != 0):
						print("Ohhh don't you dare to fuck with me")
					else:
						print("Forme réduite : {} * X^2 + {} * X^1 + {} * X^0 = 0".format(self.a, self.b, self.c))
						if self.a == 0 and self.b != 0:
							print("Équation du premier degré")
							self.first_degree()
						elif (self.a == 0 and self.b == 0 and self.c == 0):
							print("Equation du degré 0, tout les nombres reels sont une solution")
						else:
							print("Équation du second degré")
							self.second_degree(self.a, self.b, self.c)


class Calc(Parser):

	tokens = (
		'NUMBER',
		'PLUS', 'MINUS', 'TIMES',
		'LPAREN', 'RPAREN', 'X'
	)

	# Tokens

	t_PLUS = r'\+'
	t_MINUS = r'-'
	t_TIMES = r'\*'
	t_LPAREN = r'\('
	t_RPAREN = r'\)'

	def filter_results(self):
		output = {}
		degrees = self.degrees
		for degrees, times in degrees.items():
			output[degrees] = sum(times)
		self.degrees = {}
		return output

	def t_NUMBER(self, t):
		r'-?\d*\.?\d+'
		if '.' in t.value:
			t.value = float(t.value)
		else:
			t.value = int(t.value)
		return t

	def t_X(self, t):
		r'X(?:\^[0-9]+)?'
		if (t.value == 'X'):
			t.value = 'X^1'
		if (t.value == 'X^0'):
			t.value = 'X^0'
		if (int(t.value.split('^')[1]) > 2):
			print("Degree more than 2, not handling, replacing everything more than ^2 by ^2")
			t.value = 'X^2'
		return t

	t_ignore = " \t"

	def t_newline(self, t):
		r'\n+'
		t.lexer.lineno += t.value.count("\n")

	def t_error(self, t):
		print("Illegal character '%s'" % t.value[0])
		t.lexer.skip(1)

	# Parsing rules

	precedence = (
		('left', 'PLUS', 'MINUS'),
		('left', 'X'),
		('left', 'TIMES')
	)

	def p_store_degree(self, p):
		"""
		expression : NUMBER TIMES X
		"""
		self.degrees[int(p[3].split('^')[1])] += p[1]
		p[0] = 0

	def p_add(self, p):
		"""
		expression : expression PLUS NUMBER TIMES X
		"""
		self.degrees[int(p[5].split('^')[1])] += p[3]
		p[0] = 0

	def p_sub(self, p):
		"""
		expression : expression MINUS NUMBER TIMES X
		"""
		if p[1] != 0:
			print('ERROR')
			self.error = 1
		p[3] = -p[3]
		self.degrees[int(p[5].split('^')[1])] += p[3]
		p[0] = 0

	def p_X(self, p):
		"""
		expression : X
		"""
		self.degrees[int(p[1].split('^')[1])] += 1
		p[0] = 0

	def p_next_X(self, p):
		"""
		expression : expression PLUS X
					| expression MINUS X
		"""
		if p[2] == '+':
			self.degrees[int(p[3].split('^')[1])] += 1
		if p[2] == '-':
			self.degrees[int(p[3].split('^')[1])] -= 1
		p[0] = 0

	def p_number_only(self, p):
		"""
		expression : NUMBER
		"""
		self.degrees[0] += p[1]

	def p_number_add_only(self, p):
		"""
		expression : expression PLUS NUMBER
		"""
		self.degrees[0] += p[3]
	
	def p_number_minus_only(self, p):
		"""
		expression : expression MINUS NUMBER
		"""
		self.degrees[0] -= p[3]

	def p_parens(self, p):
		'expression : LPAREN expression RPAREN'
		p[0] = p[2]

	def p_error(self, p):
		print("Syntax error in input", p)
		self.error = 1

if __name__ == '__main__':
	calc = Calc()
	calc.run()