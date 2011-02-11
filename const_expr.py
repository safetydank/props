#!/usr/bin/env python

from pyparsing import Literal, Word, Regex, Combine, Optional, ZeroOrMore, Forward, \
                      nums, alphas, alphanums

import math
import re

from ppcommon import _parse_number

def _rotate_left(x, y):
    mask = 0
    for i in range(y):
        mask |= 0x80 >> i
    hi = (x << y) & 0xff
    lo = (x & mask) >> (8-y)
    return (hi | lo)

def _rotate_right(x, y):
    mask = 0
    for i in range(y):
        mask |= 0x01 << i
    hi = (x & mask) << (8-y)
    lo = (x >> y) & 0xff
    return (hi | lo)

def _limit_minimum(x, y):
    raise Exception('not implemented')

def _limit_maximum(x, y):
    raise Exception('not implemented')

def _asr(x, y):
    raise Exception('not implemented')

def _reverse(x, y):
    raise Exception('not implemented')

operations = { '+'   : lambda x, y: x + y,
               '-'   : lambda x, y: x - y,
               '*'   : lambda x, y: x * y,
               '/'   : lambda x, y: x / y,
               '//'  : lambda x, y: x % y,

               # bitwise operations, see pg391 of Propeller Manual
               '#>'  : _limit_minimum,
               '<#'  : _limit_maximum,
               '~>'  : _asr,
               '<<'  : lambda x, y: x << y,
               '>>'  : lambda x, y: x >> y,
               '&'   : lambda x, y: x & y,
               '|'   : lambda x, y: x | y,
               '^'   : lambda x, y: x ^ y,
               '<-'  : _rotate_left,
               '->'  : _rotate_right,
               '><'  : _reverse,

               # boolean operations
               'AND' : lambda x, y: -1 if bool(x and y) else 0,
               'OR'  : lambda x, y: -1 if bool(x or y) else 0,
               '=='  : lambda x, y: -1 if bool(x == y) else 0,
               '<>'  : lambda x, y: -1 if bool(x != y) else 0,
               '<'   : lambda x, y: -1 if bool(x < y) else 0,
               '>'   : lambda x, y: -1 if bool(x > y) else 0,
               '=<'  : lambda x, y: -1 if bool(x <= y) else 0,
               '=>'  : lambda x, y: -1 if bool(x >= y) else 0
             }

class ExpressionError(Exception): pass

class BoxedExpression(object):
    """An Expression container for an already evaluated term"""

    def __init__(self, expr):
        self._expr = expr
        self.stack = None

    def evaluate_stack(self):
        return self._expr
    
    def __repr__(self):
        return str(self._expr)

class Expression(object):
    """
    Helper class for handling constant expressions
        * defines the grammar for a constant expression
        * evaluates the expression
    """

    def __init__(self, initial_stack = []):
        self.stack = initial_stack

    def clear_stack(self):
        self.stack = []

    def substitute(self, lookup, postop=lambda x: x):
        """Perform variable substitution with an optional post operation"""
        self.stack = [ postop(lookup[el]) if lookup.has_key(el) else el for el in self.stack ]

    def evaluate_stack(self):
        tmp = []
        # print 'eval stack', self.stack
        for element in self.stack:
            # XXX why long?
            if isinstance(element, (int, long)):
                tmp.append(element)
            elif element in operations.keys():
                operation = operations[element]
                operand = tmp.pop()
                tmp[-1] = operation(tmp[-1], operand)
            else:
                raise ExpressionError('Unknown stack element: "%s"' % str(element))

        assert(len(tmp) == 1)
        return tmp[0]

    def pop_value(self):
        """
        Return a new expression object with existing stack and clear current
        stack

        XXX calls to this may need to be modified, a conflict between this
        and pyparsing is splitting up the expressions into fragments
        """

        new_expression = Expression(self.stack)
        self.clear_stack()

        return new_expression

    def grammar(self):
        def push_first(s, loc, toks):
            self.stack.append(toks[0])

        def push_atom(s, loc, toks):
            atom = toks[0]
            number = _parse_number(atom)
            if number is not None:
                self.stack.append(number)
            else:
                self.stack.append(toks[0].lower())

        # constant expressions
        lpar   = Literal('(').suppress()
        rpar   = Literal(')').suppress()
        addop  = Literal('+') | Literal('-')
        multop = Literal('*') | Literal('/')
        bitop  = Literal('<<') | Literal('>>') | \
                 Literal('&')  | Literal('|') | Literal('^') | \
                 Literal('<-') | Literal('->')
        number = (Combine(Literal('$') + Word(nums+'_abcdefABCDEF'))) | \
                 (Combine(Literal('%') + Word('_01'))) | \
                 Word('_' + nums)
        id = Combine(Optional('@') + Optional(':') + Regex(r'[a-zA-Z_]\w*'))

        expr = Forward()
        term = Forward()
        bwterm = Forward()

        atom  = Optional('-') + (number | id)
        rexpr = (lpar + expr.suppress() + rpar).suppress()

        # bitwise operators have highest precedence, followed by mul/div and
        # finally add/sub
        bwterm << ( atom.setParseAction(push_atom) | rexpr ) + ZeroOrMore( (bitop + bwterm).setParseAction(push_first) )
        term << bwterm + ZeroOrMore( (multop + bwterm).setParseAction(push_first) )
        expr << term + ZeroOrMore( (addop + term).setParseAction(push_first) )

        return expr
    
    def __repr__(self):
        pp = lambda x: str(x[0]) if len(x) == 1 else str(x)
        return '%s' % pp(self.stack)
    
if __name__ == '__main__':
    """Test constant expressions on the console"""

    expr = Expression()
    input = True
    while input:
        input = raw_input('-> ')
        if input:
            expr.grammar().parseString(input, parseAll=True)
            print 'Stack', expr.stack
            print expr.pop_value().evaluate_stack()

