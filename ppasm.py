#!/usr/bin/env python

"""\
ppasm.py - an assembler for the Parallax Propeller chip

Requires Python 2.5+ and the pyparsing library.

Copyright (c) 2009 Dan Venkitachalam

MIT License

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
"""

from __future__ import with_statement

# Use psyco if installed
try:
    import psyco
    psyco.full()
except:
    pass

from pyparsing import Word, alphas, Literal, Combine, Optional, nums, col, lineno, \
        ZeroOrMore, OneOrMore, alphanums, restOfLine, White, ParseException, WordEnd, Regex
from pyparsing import CaselessLiteral as L

from ppcommon import _parse_number, Record
from const_expr import Expression, BoxedExpression, ExpressionError

import os
import optparse
import sys
import re
import StringIO
import time
import traceback

CODE  = 0
DATA  = 1
LABEL = 2
PAD   = 3

# Cog RAM special purpose register addresses
COG_REGISTERS = dict(par=0x1f0,
                     cnt=0x1f1,
                     ina=0x1f2,
                     inb=0x1f3,
                     outa=0x1f4,
                     outb=0x1f5,
                     dira=0x1f6,
                     dirb=0x1f7,
                     ctra=0x1f8,
                     ctrb=0x1f9,
                     frqa=0x1fa,
                     frqb=0x1fb,
                     phsa=0x1fc,
                     phsb=0x1fd,
                     vcfg=0x1fe,
                     vscl=0x1ff,
                    )

HUB_OPERATIONS = dict(clkset=0x0,
                      cogid=0x1,
                      coginit=0x2,
                      cogstop=0x3,
                      hubop=0x0,
                      lockclr=0x7,
                      locknew=0x4,
                      lockret=0x5,
                      lockset=0x6
                     )

# opcode format
# opcode, 6-bit instr, 4 bit zcri, default 4 bit condition,  
OPCODES = { 'abs':      (0x2a, 0x02, 0xf),
            'absneg':   (0x2b, 0x02, 0xf),

            'add':      (0x20, 0x02, 0xf),
            'addabs':   (0x22, 0x02, 0xf),
            'adds':     (0x34, 0x02, 0xf),
            'addsx':    (0x36, 0x02, 0xf),
            'addx':     (0x32, 0x02, 0xf),

            'and':      (0x18, 0x02, 0xf),
            'andn':     (0x19, 0x02, 0xf),

            'call':     (0x17, 0x03, 0xf),

            'clkset':   (0x03, 0x01, 0xf),

            'cmp':      (0x21, 0x00, 0xf),
            'cmps':     (0x30, 0x00, 0xf),
            'cmpsub':   (0x38, 0x00, 0xf),
            'cmpsx':    (0x31, 0x00, 0xf),
            'cmpx':     (0x33, 0x00, 0xf),

            'cogid':    (0x03, 0x03, 0xf),
            'coginit':  (0x03, 0x01, 0xf),
            'cogstop':  (0x03, 0x01, 0xf),

            'djnz':     (0x39, 0x02, 0xf),

            'hubop':    (0x03, 0x00, 0xf),

            'jmp':      (0x17, 0x00, 0xf),
            'jmpret':   (0x17, 0x02, 0xf),

            'lockclr':  (0x03, 0x01, 0xf),
            'locknew':  (0x03, 0x03, 0xf),
            'lockret':  (0x03, 0x01, 0xf),
            'lockset':  (0x03, 0x01, 0xf),

            'max':      (0x13, 0x02, 0xf),
            'maxs':     (0x11, 0x02, 0xf),

            'min':      (0x12, 0x02, 0xf),
            'mins':     (0x10, 0x02, 0xf),

            'mov':      (0x28, 0x02, 0xf),
            'movd':     (0x15, 0x02, 0xf),
            'movi':     (0x16, 0x02, 0xf),
            'movs':     (0x14, 0x02, 0xf),

            'muxc':     (0x1c, 0x02, 0xf),
            'muxnc':    (0x1d, 0x02, 0xf),
            'muxnz':    (0x1f, 0x02, 0xf),
            'muxz':     (0x1e, 0x02, 0xf),

            'neg':      (0x29, 0x02, 0xf),
            'negc':     (0x2c, 0x02, 0xf),
            'negnc':    (0x2d, 0x02, 0xf),
            'negnz':    (0x2f, 0x02, 0xf),
            'negz':     (0x2e, 0x02, 0xf),

            'nop':      (0x0,  0x00, 0x0),

            'or':       (0x1a, 0x02, 0xf),

            'rdbyte':   (0x00, 0x02, 0xf),
            'rdlong':   (0x02, 0x02, 0xf),
            'rdword':   (0x01, 0x02, 0xf),

            'rcl':      (0x0d, 0x02, 0xf),
            'rcr':      (0x0c, 0x02, 0xf),

            'ret':      (0x17, 0x01, 0xf),

            'rev':      (0x0f, 0x02, 0xf),
            'rol':      (0x09, 0x02, 0xf),
            'ror':      (0x08, 0x02, 0xf),

            'sar':      (0x0e, 0x02, 0xf),
            'shl':      (0x0b, 0x02, 0xf),
            'shr':      (0x0a, 0x02, 0xf),

            'sub':      (0x21, 0x02, 0xf),
            'subabs':   (0x23, 0x02, 0xf),
            'subs':     (0x35, 0x02, 0xf),
            'subsx':    (0x37, 0x02, 0xf),
            'subx':     (0x33, 0x02, 0xf),

            'sumc':     (0x24, 0x02, 0xf),
            'sumnc':    (0x25, 0x02, 0xf),
            'sumnz':    (0x27, 0x02, 0xf),
            'sumz':     (0x26, 0x02, 0xf),

            'test':     (0x18, 0x00, 0xf),
            'tjnz':     (0x3a, 0x00, 0xf),
            'tjz':      (0x3b, 0x00, 0xf),

            'waitcnt':  (0x3e, 0x02, 0xf),
            'waitpeq':  (0x3c, 0x00, 0xf),
            'waitpne':  (0x3d, 0x00, 0xf),
            'waitvid':  (0x3f, 0x00, 0xf),

            'wrbyte':   (0x00, 0x00, 0xf),
            'wrlong':   (0x02, 0x00, 0xf),
            'wrword':   (0x01, 0x00, 0xf),
            
            'xor':      (0x1b, 0x02, 0xf),
          }
OPCODE_NAMES = sorted(OPCODES.keys(), reverse=True)

# reverse lookup, used for disassembly
REVERSE_OPCODES = {}
for (name, op) in OPCODES.iteritems():
    REVERSE_OPCODES[op[0]] = name

CONDITION_LOOKUP = dict(if_always    = 0xf,
                        if_never     = 0x0,
                        if_e         = 0xa,
                        if_ne        = 0x5,
                        if_a         = 0x1,
                        if_b         = 0xc,
                        if_ae        = 0x3,
                        if_be        = 0xe,
                        if_c         = 0xc,
                        if_nc        = 0x3,
                        if_z         = 0xa,
                        if_nz        = 0x5,
                        if_c_eq_z    = 0x9,
                        if_c_ne_z    = 0x6,
                        if_c_and_z   = 0x8,
                        if_c_and_nz  = 0x4,
                        if_nc_and_z  = 0x2,
                        if_nc_and_nz = 0x1,
                        if_c_or_z    = 0xe,
                        if_c_or_nz   = 0xd,
                        if_nc_or_z   = 0xb,
                        if_nc_or_nz  = 0x7,
                        if_z_eq_c    = 0x9,
                        if_z_ne_c    = 0x6,
                        if_z_and_c   = 0x8,
                        if_z_and_nc  = 0x2,
                        if_nz_and_c  = 0x4,
                        if_nz_and_nc = 0x1,
                        if_z_or_c    = 0xe,
                        if_z_or_nc   = 0xb,
                        if_nz_or_c   = 0xd,
                        if_nc_or_nc  = 0x7
                       )

CLOCK_MODES = { 'rcfast' : 0x00,
                'rcslow' : 0x01,
                'xinput' : 0x02,

                'xtal1'  : 0x2a,
                'xtal2'  : 0x32,
                'xtal3'  : 0x3a,

                'xinput+pll1x'  : 0x63,
                'xinput+pll2x'  : 0x64,
                'xinput+pll4x'  : 0x65,
                'xinput+pll8x'  : 0x66,
                'xinput+pll16x' : 0x67,

                'xtal1+pll1x'   : 0x6b,
                'xtal1+pll2x'   : 0x6c,
                'xtal1+pll4x'   : 0x6d,
                'xtal1+pll8x'   : 0x6e,
                'xtal1+pll16x'  : 0x6f,

                'xtal2+pll1x'   : 0x73,
                'xtal2+pll2x'   : 0x74,
                'xtal2+pll4x'   : 0x75,
                'xtal2+pll8x'   : 0x76,
                'xtal2+pll16x'  : 0x77,

                'xtal3+pll1x'   : 0x73,
                'xtal3+pll2x'   : 0x74,
                'xtal3+pll4x'   : 0x75,
                'xtal3+pll8x'   : 0x76,
                'xtal3+pll16x'  : 0x77,
              }

def find(sequence, predicate):
    for el in sequence:
        if predicate(el):
            return el
    return None

def make_parse_action(token):
    def _parse_action(s, loc, toks):
        print 'Called %s parse action with (%s,%s)' % (token, loc, toks)
    return _parse_action

def make_fail_action(token):
    def _fail_action(s, loc, expr, err):
        print 'Failed parse action with (%s,%s,%s)' % (loc, expr, err)
    return _fail_action

def wordlist(*args):
    return reduce(lambda x, y: x | y, [ L(arg) for arg in args ])

# Exception classes
class ParserError(Exception): pass
class SubstituteError(Exception): pass

class Section(object):
    """ Code/data section, delimited by org directives  """

    def __init__(self):
        self.content = []

    def add_to_pool(self):
        pass
    
class ConstantPool(object):
    """ work in progress """

    def __init__(self):
        self._reset()

    def _reset(self):
        self._lookup = dict()

    def add_constant(self, name, value):
        self._lookup[name] = value
        if not self._lookup.has_key(value):
            self._lookup[value] = []
        self._lookup[value].append(name)
    
    def to_record(self):
        Record(pool = self)

    def translate(self):
        pass

class Assembler(object):
    def __init__(self):
        self.program = []
        self.addresses = { }
        self.hub_addresses = { }

        # internals

        # _pc tracks the current cog register address (in bytes, max 2047)
        self._pc = 0
        # _ram_address tracks the current RAM address offset (in bytes, max 32767)
        self._ram_address = 0

        # current code section, used during parsing
        self._section = None
        self._sections = []

        self._labels = []
        self._last_global_label = '__initial'

        self._const_expr = Expression()
        self._data = []
        self._string = None
        self._immediate = None
        self._condition = None

        self._call_data = []

        self._reset_source_dest()

        self._xinfreq = 10**6   # default - 1 mhz
        self._clkmode = 0       # internal oscillator

    def _reset_source_dest(self):
        self.source = None
        self.dest = None

    @property
    def size(self):
        return self._ram_address

    def parse(self, input):
        t = time.time()
        bnf = self.grammar()

        bnf.parseString(input, parseAll=True)

        if self._section:
            # append last section
            self._sections.append(self._section)
            self._section = None
        
        # check call labels
        for call in self._call_data:
            label = call['branch_label']

            # should be matching ret label and instruction

            return_label = label.lower() + '_ret'
            ret_address = self.addresses.get(return_label)

            if ret_address is None:
                raise ParserError('Error: missing ret label "%s_ret"' % label)

            # rewrite call's dest to point to ret_address
            call_address = call['call_address']
            (addr, raddr, type, call_opcode) = find(self.program, 
                                                    lambda el: el[0] == call_address)
            call_opcode.dest = Expression([ret_address / 4])

            # XXX the following check is flawed because it checks ALL code 
            # sections for a matching ret instruction
            #
            # it should only check the local code section, to prevent (wrongly)
            # matching a ret instruction in another section.
            #
            # the best way to fix this is to introduce the concept of code
            # sections, also required for constant pools.
            #
            # and write a test case to verify it is working correctly
            if not find(self.program, lambda (addr, raddr, type, opcode): \
                                                 addr == ret_address and \
                                                 type == CODE and \
                                                 opcode.instr == 'ret'):
                raise ParserError('Error: no ret instruction at label "%s_ret"' % label)

    def make_parse_label(self):
        def _check_label(name):
            err = None

            if self.addresses.get(name):
                err = 'Label "%s" defined more than once' % name
            elif COG_REGISTERS.get(name):
                err = 'Label "%s" shadows a built in register name' % name

            if err:
                raise ParserError(err)

        def parse_label(s, loc, toks):
            label = toks[0].lower()

            if label[0] == ':':
                # mangle local labels with last global label name
                label = self._last_global_label + label

            self._labels.append(label)
            _check_label(label)
            
        return parse_label

    def make_parse_op(self):
        def _parse_op(s, loc, toks):
            line, column = lineno(loc, s), col(loc, s)
            instr   = toks[0].lower()
            effects = self._effects

            # check for valid opcode
            opcode = OPCODES.get(instr)
            if not opcode:
                raise ParserError('Syntactically valid but undefined opcode %s' % instr)

            if instr == 'call':
                if not self._immediate:
                    raise ParserError('Line %d:%d Call to label without "#"' % (line, column))

                assert(self.source)
                label = self.source.stack[0]

                self._call_data.append(dict(call_address=self._pc,
                                            branch_label=label,
                                            ))
            # hub operation
            if opcode[0] is 0x03:
                source = HUB_OPERATIONS.get(instr)
                if not source:
                    raise ParserError('Undefined hub operation "%s"' % instr)
                self.source = Expression([source])
            elif instr == 'jmp':
                # HACK - self.dest should not be set for jmp (grammar workaround)
                self.dest = None  

            self.add_to_program(CODE, Record(instr=instr, 
                                             dest=self.dest, 
                                             source=self.source, 
                                             effects=effects, 
                                             condition=self._condition,
                                             immediate=self._immediate,
                                             line=line))
            self._reset_source_dest()
            self._immediate = None

        return _parse_op

    def make_parse_directive(self):
        def _parse_directive(s, loc, toks):
            line, column = lineno(loc, s), col(loc, s)
            directive = toks[0].lower()

            if directive == 'org':
                # supports org 0 only
                if len(toks) > 1 and _parse_number(toks[1]) != 0:
                    raise ParserError('Error: this assembler does not support a non-zero argument to ORG')
                # create a new section, add old one to program sections
                if self._section:
                    self._sections.append(self._section)
                self._section = Section()

                self._align_pc_to_boundary()
                self._pc = 0

            elif directive == 'res':
                # align reserved longs to long boundaries
                self._align_pc_to_boundary()
                size = _parse_number(toks[1]) * 4
                self.add_to_program(PAD, Record(padtype='res', 
                                                size=size, 
                                                line=line), size)

            elif directive == 'fit':
                fit_size = _parse_number(toks[1]) * 4
                # print 'FIT %d PC %d' % (fit_size, self._pc)

                if self._pc >= fit_size:
                    raise ParserError('Error: fit requested %d longs, %d longs used' % (fit_size / 4, self._pc / 4))

            elif directive == '.xinfreq':
                # clock frequency
                self._xinfreq = _parse_number(toks[1])

            elif directive == '.clkmode':
                # clock mode
                mode = ''.join(toks[1:]).lower()
                clkmode = CLOCK_MODES.get(mode)

                if clkmode is None:
                    raise ParserError('Unrecognized clkmode directive: %s' % mode)
                self._clkmode = clkmode

            elif directive == '.horg':
                # hub org
                org = _parse_number(toks[1])
                self._align_pc_to_boundary()
                # XXX this is a really primitive placeholder, we'll have to 
                # refactor the way programs are stored to support overlays
                padding = org - self._ram_address
                assert(padding >= 0)
                if padding:
                    self.add_to_program(PAD, Record(padtype='_pad', 
                                                    size=padding,
                                                    line=None), padding, False)
                self._ram_address = org

            elif directive == '.hfit':
                # hub fit
                fit = _parse_number(toks[1])
                if self._ram_address >= fit:
                    raise ParserError('Error: hfit address is %d but section ends at %d' % (
                        fit, self._ram_address))

        return _parse_directive

    def _data_size(self, tok):
        return dict(long=4, word=2, byte=1).get(tok, 0)

    def make_parse_data(self):
        def parse_data(s, loc, toks):
            line, column = lineno(loc, s), col(loc, s)

            datatype = toks[0]
            size = self._data_size(datatype)

            if self._const_expr.stack:
                self._data.append(self._const_expr.pop_value())

            if self._string:
                self._data.append(self._string)

            data = []
            for el in self._data:
                if isinstance(el, str):
                    # translate string constants to an expression list
                    for ch in el:
                        data.append(BoxedExpression(ord(ch)))
                else:
                    data.append(el)

            if not data:
                raise ParserError('Missing expressions for data: %s' % str(toks))

            self._data = []
            self._align_pc_to_boundary(size)

            self.add_to_program(DATA, Record(datatype=datatype, 
                                             content=data,
                                             line=line), size  * len(data))
        return parse_data

    def _align_pc_to_boundary(self, boundary=4):
        mod = self._pc % boundary
        if mod:
            padding = boundary - mod
            self.add_to_program(PAD, Record(padtype='_pad', 
                                            size=padding,
                                            line=None), padding, False)

    def _sub_expr_locals(self, id, record):
        """
        Substitute local label names with their mangled counterparts
        inside expressions.  Checks code data and source operands, and
        data directive expressions
        """

        def _sub(expr):
            if expr.stack:
                new_stack = []
                for el in expr.stack:
                    if isinstance(el, str) and el.startswith(':'):
                        el = self._last_global_label + el
                    new_stack.append(el)
                expr.stack = new_stack

        if id == CODE:
            if record.source:
                _sub(record.source)
            if record.dest:
                _sub(record.dest)
        elif id == DATA:
            if record.content:
                for el in record.content:
                    _sub(el)

    def add_to_program(self, id, record, size=4, resolve_label=True):
        if resolve_label and self._labels:
            # resolve current label reference
            for label in self._labels:
                self.addresses[label] = self._pc
                self.hub_addresses['@%s' % label] = self._ram_address
                if ':' not in label:
                    self._last_global_label = label

        self._sub_expr_locals(id, record)

        self.program.append((self._pc, self._ram_address, id, record))
        assert(self._section)
        self._section.content.append((self._pc, self._ram_address, id, record))

        self._pc += size
        self._ram_address += size

    def grammar(self):
        """
        Defines the grammar of a Propeller assembler source file
        """

        def _set_immediate():
            self._immediate = True

        def parse_source():
            self.source = self._const_expr.pop_value()

        def parse_dest():
            self.dest = self._const_expr.pop_value()

        def parse_effect(s, loc, toks):
            self._effects.append(toks[0])

        # set and unset condition flags
        def parse_condition(s, loc, toks):
            assert(toks)
            cond = CONDITION_LOOKUP.get(toks[0].lower())
            if cond:
                self._condition = cond
            else:
                raise Exception('Unknown condition %s' % toks[0])

        def parse_line(s, loc, toks):
            self._condition = None
            self._labels = []
            self._effects = []

        def parse_expr_comma():
            if self._const_expr.stack:
                self._data.append(self._const_expr.pop_value())
            elif self._string:
                self._data.append(self._string)
                self._string = None

        def parse_string(s, loc, toks):
            assert(toks[0][0] == toks[0][-1] == '"')
            self._string = toks[0][1:-1]

        comment = Combine(Literal("'") + restOfLine)

        identifier = Word(alphanums+'_')
        number = (Combine(Literal('$') + Word(nums+'_abcdefABCDEF'))) | \
                 (Combine(Literal('%') + Word('_01'))) | \
                 Word('_' + nums)
        expr_comma = Literal(',')
        comma = Literal(',')
        ws = White().suppress()

        expr_comma.setParseAction(parse_expr_comma)

        # constant expressions
        const_expr = self._const_expr.grammar()
        expr_or_string = const_expr | Regex('".*"').setParseAction(parse_string)

        long_data = L('long') + ws + const_expr + Optional(ZeroOrMore(expr_comma + const_expr))
        word_data = L('word') + ws + const_expr + Optional(ZeroOrMore(expr_comma + const_expr))
        # byte data elements can be either an expression or a string
        byte_data = L('byte') + ws + expr_or_string + Optional(ZeroOrMore(expr_comma + expr_or_string))
        data = long_data | word_data | byte_data

        org_directive = L('org') + Optional(number)
        fit_directive = L('fit') + number
        res_directive = L('res') + number

        # custom directives
        _xinfreq_directive = L('.xinfreq') + number
        _clkmode_directive = L('.clkmode') + identifier + Optional(Literal('+') + identifier)
        _horg_directive = L('.horg') + number
        _hfit_directive = L('.hfit') + number

        operand = const_expr
        # this works but Combine(Optional(L('#') +op...) doesn't (?!?!?!)

        source  = Optional(Literal('#').setParseAction(_set_immediate)) + operand
        dest    = ~Literal('#') + operand

        opcode = wordlist(*OPCODE_NAMES)
        effect = wordlist('nr', 'wr', 'wc', 'wz') + WordEnd()
                 
        op = ( L('ret') | L('nop') ) | \
                (opcode + WordEnd() + Optional(dest.setParseAction(parse_dest) + comma) \
                                    + source.setParseAction(parse_source) \
                                    + ZeroOrMore(effect.setParseAction(parse_effect)))

        directive = org_directive | fit_directive | res_directive |\
                _xinfreq_directive | _clkmode_directive | _horg_directive | _hfit_directive

        extended_directive = Literal('.') + identifier + \
                             Optional(operand + ZeroOrMore(comma + operand))

        op_or_directive = directive \
                | data.setParseAction(self.make_parse_data()) \
                | op.setParseAction(self.make_parse_op())
        condition = Combine(L('if_') + identifier) + ws

        # try to match condition or op_or_directive before label
        global_label = identifier
        local_label  = Combine(Literal(':') + identifier)
        label = local_label | (~condition + ~op_or_directive + global_label)
        line = ZeroOrMore(label.setParseAction(self.make_parse_label())) \
                + Optional(condition.setParseAction(parse_condition)) \
                + op_or_directive
        line.setParseAction(parse_line)
        program = OneOrMore( line ) 

        data.setParseAction(self.make_parse_data())
        directive.setParseAction(self.make_parse_directive())

        bnf = program
        bnf.ignore(comment)

        return bnf

    def _offset_hub_addresses(self, offset):
        """Add an offset to internal hub addresses (e.g. preamble length)"""

        offset_addresses = dict()
        for (k,v) in self.hub_addresses.iteritems():
            offset_addresses[k] = v + offset
        return offset_addresses

    def substitute(self, hub_offset=0):
        """Perform address substitution"""

        def _check_address(name, address):
            err = None

            if address is None:
                err = 'Missing label definition for "%s"' % name
            elif address % 4:
                err = 'Label "%s" at address $%x is not long aligned' % (name, address)

            if err:
                raise SubstituteError(err)

        hub_addresses = self._offset_hub_addresses(hub_offset) if hub_offset else self.hub_addresses

        for (cog_address, ram_address, id, data) in self.program:
            if id is CODE:
                # Translate from internal cog byte address to
                # cog long addressing
                byte2long = lambda addr: addr / 4

                # perform all substitutions
                for operand in (data.source, data.dest):
                    if operand is not None:
                        operand.substitute(COG_REGISTERS)
                        operand.substitute(self.addresses, byte2long)
                        operand.substitute(hub_addresses)
            elif id is DATA:
                for expr in data.content:
                    if expr.stack:
                        expr.substitute(COG_REGISTERS)
                        expr.substitute(self.addresses, byte2long)
                        expr.substitute(hub_addresses)

    def translate(self, out):
        """
        Translate program to machine code

        out -- a file stream to write the output to
        """

        def _write_bytes(seq):
            out.write(''.join(chr(x) for x in seq))

        def _zcr_from_effects(effects, default_zcr=0):
            zcr = default_zcr

            if 'nr' in effects:
                zcr &= 0x6
            if 'wr' in effects:
                zcr |= 0x1
            if 'wc' in effects:
                zcr |= 1 << 1
            if 'wz' in effects:
                zcr |= 1 << 2

            return zcr

        for (address, ram_address, id, data) in self.program:
            if id is CODE:
                # default opcode values
                (opcode, zcri, default_cond) = OPCODES[data.instr]

                zcr = zcri >> 1
                if data.effects:
                    zcr = _zcr_from_effects(data.effects, zcr)

                # set default destination flags
                dest = 0 if data.dest is None else data.dest.evaluate_stack()
                # set default source flags
                source = 0 if data.source is None else data.source.evaluate_stack()
                # set default condition flags
                cond = default_cond if data.condition is None else data.condition

                # check immediate flag is set - if not apply default value
                if not data.immediate and bool(zcri & 1):
                    data.immediate = True
                imm = 1 if data.immediate else 0

                is_immediate_operand = lambda x: (x & 0x1ff) == x
                if imm:
                    msg = 'Line %d: %s operand does not fit into 9-bit immediate address'
                    if not is_immediate_operand(source):
                        raise ParserError(msg % (data.line, 'source'))
                    elif not is_immediate_operand(dest):
                        raise ParserError(msg % (data.line, 'dest'))

                # form 4-byte opcode 
                bytes = [ (opcode << 2) | (zcr >> 1),
                          ((zcr & 1) << 7) | (imm << 6) | (cond << 2) | (dest >> 7),
                          ((dest & 0x7f) << 1) | (source >> 8),
                          source & 0xff ]

                # write opcode bytes out in reverse order
                try:
                    _write_bytes(bytes[::-1])
                except ValueError, e:
                    raise ParserError('Line %d: %s' % (data.line, str(e)))

            elif id is DATA:
                size = self._data_size(data.datatype)
                # evaluate data expressions
                for expression in data.content:
                    v = expression.evaluate_stack()
                    if isinstance(v, list):
                        bytes = v
                    else:
                        bytes = [ (v >> 8*n) & 0xff for n in range(size) ]

                    _write_bytes(bytes)

            elif id is PAD:
                for i in range(data.size):
                    out.write('\x00')

    @property
    def preamble(self):
        """
        Returns a minimal preamble template.

        Minimal Spin bootstrap code for assembly language launch
        --------------------------------------------------------
        $0000: HZ HZ HZ HZ CR CS 10 00 LL LL 18 00 18 00 10 00
        $0010: FF FF F9 FF FF FF F9 FF 35 37 04 35 2C -- -- --
        $0020: your assembly code starts here - loaded into COG #0

        elaboration:
        $0000: HZ HZ HZ HZ - internal clock frequency in Hz (long)
        $0004: CR          - value to be written to clock register (byte)
        $0005: CS          - checksum so that all RAM bytes will sum to 0 (modulus 256)
        $0006: 10 00       - 'pbase' (word) must be $0010
        $0008: LL LL       - 'vbase' (word) number of longs loaded times 4
        $000A: 18 00       - 'dbase' (word) above where $FFF9FFFF's get placed
        $000C: 18 00       - 'pcurr' (word) points to Spin code
        $000E: 10 00       - 'dcurr' (word) points to local stack
        $0010: FF FF F9 FF - below local stack, must be $FFF9FFFF
        $0014: FF FF F9 FF - below local stack, must be $FFF9FFFF
        $0018: 35          - push #0   (long written to $0010)
        $0019: 37 04       - push #$20 (long written to $0014)
        $001B: 35          - push #0   (long written to $0018)
        $001C: 2C          - COGINIT(0, $20, 0) - load asm code from $20+ into same COG #0
        $001D: -- -- --    - filler
        $0020: XX XX XX XX - 1st long of asm program to be loaded into COG #0
        $0024: XX XX XX XX - 2nd long of asm program to be loaded into COG #0
        $0028:             - rest of data
        """

        return ''.join(('\x00\x00\x00\x00', # HZ HZ HZ HZ
                        '\x00\x00',         # CR CS
                        '\x10\x00',
                        '\x00\x00',         # LL LL
                        '\x18\x00',
                        '\x18\x00',
                        '\x10\x00',
                        '\xff\xff\xf9\xff',
                        '\xff\xff\xf9\xff',
                        '\x35',
                        '\x37\x04',
                        '\x35',
                        '\x2c',
                        '\x00\x00\x00'      # filler
                      ))

    def write_preamble(self, output):
        output.write(self.preamble)

    def fill_preamble(self, output):
        def _write_long(long):
            for i in range(0,32,8):
                output.write(chr((long >> i) & 0xff))

        def _write_word(word):
            for i in range(0,16,8):
                output.write(chr((word >> i) & 0xff))

        output.seek(0)

        _write_long(self._xinfreq)
        output.write(chr(self._clkmode))

        output.seek(8)
        _write_word(self.size)

    def dump_addresses(self):
        print 'ADDRESSES (cog)'
        print '\n'.join([str(a) for a in self.addresses.iteritems()])
        print 'ADDRESSES (hub)'
        print '\n'.join([str(a) for a in self.hub_addresses.iteritems()])

    def dump_program(self):
        so = StringIO.StringIO()
        so.write('PROGRAM\n')
        for (addr, hubaddr, linetype, rec)  in self.program:
            so.write('%3d|%-5d L%-6d ' % (addr/4, hubaddr, rec.line or 0))
            if linetype == CODE:

                try:
                    src = rec.source.evaluate_stack()
                    if rec.dest: dest = rec.dest.evaluate_stack()
                except (AttributeError, ExpressionError):
                    src, dest = str(rec.source), str(rec.dest)

                so.write('%s %s,%s%s %s %s\n' % (rec.instr, dest, 
                                                     '#' if rec.immediate else '',
                                                     src,
                                                     rec.effects or '',
                                                     rec.condition or ''))
            elif linetype == DATA:
                pp = lambda x: str(x[0]) if len(x) == 1 else str(x)
                so.write('%s %s\n' % (rec.datatype, pp(rec.content)))
            elif linetype == PAD:
                if rec.padtype == '_pad':
                    so.write('pad %d bytes\n' % rec.size)
                elif rec.padtype == 'res':
                    so.write('res %d long%s\n' % (rec.size / 4, 's' if rec.size > 4 else ''))
            else:
                so.write('\n')

        so.write('Sections: %d' % len(self._sections))
        return so.getvalue()

        # print '\n'.join([str(i) for i in self.program])

    def dump_substitute(self):
        print 'SUBSTITUTE'
        print '\n'.join([str(i) for i in self.program])

def checksum(str):
    """
    Calculate checksum byte for a program string
    """

    sum = reduce(lambda x, y: x+y, [ ord(byte) for byte in str ]) & 0xff
    cs = 256 - sum if sum % 256 else 0

    return chr(cs)

def dis(bytes):
    """
    Disassemble a 4 byte assembler hex opcode

    This method takes the opcode in the same order it is written to the EEPROM
    file (big endian).

    TODO does not correctly disassemble hub operations

    e.g. dis('0fecbfa0')
    """

    bytes = bytes.replace(' ', '')
    assert(len(bytes) == 8)

    byte0 = int(bytes[6:8], 16) & 0xFF
    byte1 = int(bytes[4:6], 16) & 0xFF
    byte2 = int(bytes[2:4], 16) & 0xFF
    byte3 = int(bytes[0:2], 16) & 0xFF

    opcode = byte0 >> 2
    zcr    = ((byte0 & 0x3) << 1) | (byte1 >> 7)
    imm    = (byte1 >> 6) & 0x1
    cond   = (byte1 >> 2) & 0xf
    dest   = ((byte1 & 0x3) << 7) | (byte2 >> 1)
    source = ((byte2 & 0x01) << 8) | byte3

    print 'Opcode: %d (%s)' % (opcode, REVERSE_OPCODES.get(opcode, -1))
    print '  zcr : 0x%x' % zcr
    print '  imm : 0x%x' % imm
    print ' cond : 0x%x' % cond
    print ' dest : 0x%x %d' % (dest, dest)
    print '  src : 0x%x %d' % (source, source)

if __name__ == '__main__':

    def _chext(filename, newext):
        """Change filename extension"""

        head = filename.rpartition('.')[0] if '.' in filename else filename
        return ''.join((head, '.', newext))

    if len(sys.argv) not in (2,3):
        print 'Usage: ppasm.py <source file> [<eeprom output>]'
    else:
        sourcefile = sys.argv[1]
        dest = sys.argv[2] if len(sys.argv) == 3 else _chext(sourcefile, 'eeprom')

        try:
            elapsed_since = lambda t: ('%.6f' % (time.time() - t)).rstrip('0') + 's'

            begin = t = time.time()
            assembler = Assembler()

            print 'Assembler source:', sourcefile, '\n'
            with file(sourcefile, 'r') as fp:
                assembler.parse(fp.read())

            print 'Parse      :', elapsed_since(t)
            # assembler.dump_addresses()
            # print assembler.dump_program()

            t = time.time()
            assembler.substitute(hub_offset=len(assembler.preamble))
            print 'Substitute :', elapsed_since(t)

            t = time.time()

            output = StringIO.StringIO()

            assembler.write_preamble(output)
            assembler.translate(output)

            print assembler.dump_program()
            assembler.fill_preamble(output)

            program_size = len(output.getvalue())
            assert(program_size < 32768)

            # seek to end of stream and pad with 0's
            pad_length = 32768 - program_size
            output.seek(0, mode=2)
            output.write('\x00' * pad_length)

            # write the checksum
            check = checksum(output.getvalue())
            output.seek(5)
            output.write(check)

            if os.path.exists(dest):
                os.remove(dest)

            with file(dest, 'wb') as fp:
                fp.write(output.getvalue())
                fp.close()

            print 'Translate  :', elapsed_since(t)
            print 'Total      :', elapsed_since(begin), '\n'

            print 'Output written to', dest
            print 'Program size: %d bytes (%d longs, %.1f%% EEPROM)' % (program_size, 
                    program_size >> 2, program_size / 32768.0 * 100.0)
        except ParseException, e:
            # line, col = lineno(e.loc, e.pstr), col(e.loc, e.pstr)
            print 'Syntax error:', str(e)

            inputline = e.markInputline()
            line_length = len(inputline)
            print inputline[0:80], 
            print ' (%d more bytes) ' % (line_length - 80) if line_length > 80 else ''
        except Exception, e:
            traceback.print_exc(file=sys.stdout)

