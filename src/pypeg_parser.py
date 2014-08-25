# -*- coding: utf-8 -*-
# Necessary imports for pypeg2
from __future__ import unicode_literals, print_function
from pypeg2 import *


class DUTParser(object):
    def __init__(self, dut):
        self.dut = dut

    def parse(self):
        pass

class Direction(Keyword):
    grammar = Enum(Keyword('in'), Keyword('out'), Keyword('inout'), Keyword('buffer'))

class Type(Keyword):
    grammar = Enum(Keyword('std_logic'), Keyword('bit'), Keyword('std_logic_vector'))
    
class SigVar(Keyword):
    grammar = Enum(Keyword('signal'), Keyword('variable'))
    
class Parameter(Plain):
    grammar = attr('sigvar', SigVar), name(), ':', attr('direction', Direction), attr('type', Type)

class Parameters(List):
    grammar = Parameter, maybe_some(";", Parameter)

#class Header(Plain):
#    grammar = Enum(Function, Procedure, SigVar)
#
#class Headers(List):
#    grammar = Header, maybe_some(Header)

class Entity(Plain):
    grammar = "entity", name(), "is", "end", optional("entity"), optional(name()), ";"

class Procedure(Plain):
    grammar = "procedure", name(), "(", attr(Parameters), ")", "is", "begin","end", optional("procedure"), optional(name()), ";"
    
class Function(Plain):
    grammar = "function", name(), "(", attr(Parameters), ")", "is", "begin","end", optional("function"), optional(name()), ";"
    
class Architecture(Plain):
    grammer = "architecture", name(), "of", word, "is", attr(Headers), "begin", maybe_some(word), "end", optional("architecture"), optional(name()), ";" 


example_dut = """
comments
libraries
imports
entity declaration
entity end

architecture declaration            #Multiple architectures possible!
architecture prebody        
functions & procedures here         #Multiple possible!
architecture "begin"
architecture body
call to functions & procedures      
architecture end
"""

if __name__ == '__main__':
    entity = parse(example_dut, Entity)
    for sig in entity.port:
        print(sig.name)