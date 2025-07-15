import re

########################################################################
# 1. Lexer
########################################################################
TOKEN_SPEC = [
    ('NUMBER', r'\d+'),
    ('PRINT', r'print'),
    ('ID', r'[A-Za-z_][A-Za-z0-9_]*'),
    ('ASSIGN', r'='),
    ('SEMICOL', r';'), # semicolon
    ('OP', r'[+\-*/]'), # operators + \ - *
    ('LPAREN', r'\('), # left parenthesis
    ('RPAREN', r'\)'), # right parenthesis
    ('SKIP', r'[ \t\n]+'), # space or tab or new line
    ('MISMATCH', r'.'), # any (other) letter
]

TOKEN_REGEX = '|'.join(f'(?P<{n}>{p})' for n, p in TOKEN_SPEC)
# (?P<NUMBER>\d+)|(?P<PRINT>print)|(?P<ID>[A-Za-z_][A-Za-z0-9_]*)|(?P<ASSIGN>=)|(?P<SEMICOL>;)|(?P<OP>[+\-*/])|(?P<LPAREN>\()|(?P<RPAREN>\))|(?P<SKIP>[ \t\n]+)|(?P<MISMATCH>.)

def lexer(code: str):
    for m in re.finditer(TOKEN_REGEX, code): # re.finditer(pattern, string) finds pattern inside string
        kind = m.lastgroup # token type
        val = m.group() # lexeme value
        if kind == 'NUMBER':
            yield ('NUMBER', int(val))
        elif kind in ('ID', 'PRINT', 'OP', 'ASSIGN',
                      'SEMICOL', 'LPAREN', 'RPAREN'):
            yield (kind, val)
        elif kind == 'SKIP':
            continue
        else:
            raise SyntaxError(f'Unexpected {val!r}')

########################################################################
# 2. Parser
########################################################################
class Parser:
    def __init__(self, tokens):
        self.tok = list(tokens)
        self.pos = 0

    # helper functions
    def _peek(self):
        return self.tok[self.pos] if self.pos < len(self.tok) else ('EOF', None)
    def _pop(self):
        tok = self._peek()
        self.pos += 1
        return tok
    def _expect(self, typ):
        tok, _ = self._pop()
        if tok != typ:
            raise SyntaxError(f'Expected {typ}, got {tok}')

    # Syntax:
    # program ::= stmt*
    # stmt    ::= ID '=' expr ';' | 'print' expr ';'
    # expr    ::= term (('+'|'-') term)*
    # term    ::= factor (('*'|'/') factor)*
    # factor  ::= NUMBER | ID | '(' expr ')'
    def parse_program(self):
        stmts = []
        while self._peek()[0] != 'EOF':
            stmts.append(self._parse_stmt())
        return ('PROGRAM', stmts)

    def _parse_stmt(self):
        tok, val = self._peek()
        if tok == 'ID':
            # stmt ::= ID '=' expr ';'
            var = val; self._pop()
            self._expect('ASSIGN') # '='
            expr = self._parse_expr() # expr
            self._expect('SEMICOL') # ';'
            return ('ASSIGN', var, expr)
        elif tok == 'PRINT': 
            # stmt ::= PRINT expr ';'
            self._pop()
            expr = self._parse_expr() # expr
            self._expect('SEMICOL') # ';'
            return ('PRINT', expr)
        else:
            raise SyntaxError('Invalid statement')

    def _parse_expr(self):
        # expr ::= term (('+'|'-') term)*
        node = self._parse_term() # term
        while self._peek() == ('OP', '+') or self._peek() == ('OP', '-'):
            # '+' | '-' term
            op = self._pop()[1]
            right = self._parse_term()
            node = ('BINOP', op, node, right)
        return node

    def _parse_term(self):
        # term ::= factor (('*'|'/') factor)*
        node = self._parse_factor() # factor
        while self._peek() == ('OP', '*') or self._peek() == ('OP', '/'):
            # '*' | '/' factor
            op = self._pop()[1]
            right = self._parse_factor()
            node = ('BINOP', op, node, right)
        return node

    def _parse_factor(self):
        # factor ::= NUMBER | ID | '(' expr ')'
        tok, val = self._peek()
        if tok == 'NUMBER':
            self._pop()
            return ('NUM', val)
        elif tok == 'ID':
            self._pop()
            return ('VAR', val)
        elif tok == 'LPAREN':
            self._pop()
            expr = self._parse_expr() # expr
            self._expect('RPAREN')
            return expr
        raise SyntaxError('Expected NUMBER, VAR or "("')

########################################################################
# 3. Code Generator
########################################################################
def compile_program(ast):
    bc = [] # bc = [(op, arg?), ...]

    # helper function
    def emit(op, *args):
        bc.append((op, *args))

    def gen_expr(node):
        typ = node[0]
        if typ == 'NUM':
            emit('PUSH', node[1])
        elif typ == 'VAR':
            emit('LOAD', node[1])
        elif typ == 'BINOP':
            _, op, left, right = node
            gen_expr(left)
            gen_expr(right)
            emit({'+' : 'ADD', '-' : 'SUB',
                  '*' : 'MUL', '/' : 'DIV'}[op])
        else:
            raise ValueError('unknown expr')

    for stmt in ast[1]:
        kind = stmt[0]
        if kind == 'ASSIGN':
            _, var, expr = stmt
            gen_expr(expr)
            emit('STORE', var)
        elif kind == 'PRINT':
            _, expr = stmt
            gen_expr(expr)
            emit('PRINT')
    return bc

########################################################################
# 4. VM
########################################################################
def run(bytecode):
    stack, env, pc = [], {}, 0
    while pc < len(bytecode):
        op, *arg = bytecode[pc]; pc += 1
        if op == 'PUSH':   stack.append(arg[0])
        elif op == 'LOAD': stack.append(env.get(arg[0], 0))
        elif op == 'STORE': env[arg[0]] = stack.pop()
        elif op == 'ADD':  b, a = stack.pop(), stack.pop(); stack.append(a + b)
        elif op == 'SUB':  b, a = stack.pop(), stack.pop(); stack.append(a - b)
        elif op == 'MUL':  b, a = stack.pop(), stack.pop(); stack.append(a * b)
        elif op == 'DIV':  b, a = stack.pop(), stack.pop(); stack.append(a // b)
        elif op == 'PRINT': print(stack.pop())

########################################################################
# 5. main function
########################################################################
if __name__ == '__main__':
    source = """ x = 3 + 4; print x + 1; """
    tok = lexer(source) # Tokenize
    ast = Parser(tok).parse_program() # Parse
    bc = compile_program(ast) # Code generation
    run(bc) # Run