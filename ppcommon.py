import re

class Record(object):
    """
    Basic dict wrapper with attributes accessible as properties
    """

    def __init__(self, **kw):
        self.__dict__['internal_dict'] = dict(**kw)
    
    def __getattr__(self, key):
        return self.__dict__['internal_dict'][key]
    
    def __setattr__(self, key, value):
        self.__dict__['internal_dict'][key] = value
    
    def __delattr__(self, key):
        del self.__dict__['internal_dict'][key]

    def __repr__(self):
        dct = self.__dict__['internal_dict']
        return ' '.join([ '%s:%s' % (k[0:3], v) for k,v in dct.iteritems() ])

def _parse_number(tok):
    """
    Parse a number in decimal, hexadecimal and binary forms with
    optional underscore separators.

    e.g. 101, $ff_00_ff_00, %1111_0000_1111_0000

    Returns parsed number as an integer, or None if unable to parse
    """

    numstr = tok.replace('_', '')
    value = 0

    if numstr.isdigit():
        value = int(numstr)
    elif re.match(r'^\$[0-9a-f]+', numstr, re.IGNORECASE):
        value = int(numstr[1:], 16)
    elif numstr.startswith('%') and numstr[1:].isdigit():
        value = int(numstr[1:], 2) 
    else:
        # not a recognisable number value
        value = None
    
    return value


