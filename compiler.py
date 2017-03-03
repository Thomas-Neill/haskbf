#!/usr/bin/env python3
import re
import operator
import math
import copy
import argparse
from bfgenerator import *

#===== CLASSES =====#

class Function:
    def __init__(self,args,code,enviroment):
        self.masterCode = code
        self.args = args #names of args
        self.local = copy.copy(enviroment)
    def __call__(self,*args):
        code = copy.deepcopy(self.masterCode)
        for i in range(len(self.args)):
            self.local[self.args[i]] = args[i]
        for statement in code[:-1:]:
            evaluate(statement,self.local)
        return evaluate(code[-1],self.local)

class BadSyntax(Exception):
    def __init__(self,code):
        self.code = code
    def __str__(self):
        return("Could not detect atom or list: {}".format(self.code))

class MissingParentheses(Exception):
    def __str__(self):
        return("Error: Lack of parentheses")
#===== STANDARD FUNCTIONS =====#
    
generator = BFGenerator()
def let(name,value):
    standard[name] = value

def genList(*args):
    return args

def add(v1,v2):
    if(tp(v1) == STR or tp(v2) == STR):
        return generator.concat(v1,v2)
    else:
        return v1 + v2

def delete(name):
    del standard[name]

standard = { #our 'standard' library
    'str':0,
    'let':let,
    'debug':print,
    'print':generator.puts,
    '+':add,
    'alloc':generator.alloc
}

#===== RUNNING THE CODE =====#

def split(text):
    ret = []
    text = re.sub(r'^\s+','',text) #strips leading whitespace
    token = ''
    checkingSpaces = True
    whitespace = '\n\t '
    while(True):
        if(len(text) == 0):
            return []
        char = text[0]
        if(char in whitespace and checkingSpaces):
            return [token] + split(text)
        if(char in ''''"'''):
            checkingSpaces = not checkingSpaces
        token += char
        text = text[1::]

def splitNested(code,start='(',end=')'):
    code = split(code.replace(start,' ' + start + ' ').replace(end,' ' + end + ' '))
    return recursiveSplit(code,start,end)

def recursiveSplit(code,start,end):
    token = code.pop(0)
    if(token == start):
        ret = []
        while(code[0] != end):
            ret.append(recursiveSplit(code,start,end))
        code.pop(0)
        return ret
    elif(token == end):
        raise MissingParentheses()
    else:
        return token
            
def run(code,names):
    code = splitNested(code)
    for expr in code:
        evaluate(expr,names)

def evaluate(expression,names):
    if(isinstance(expression,list)):
        args = []
        if(expression[0] == 'let'):
            assert not (re.match(r'\d+',expression[1]) or re.match(r'".+"',expression[1]))
            return let(expression[1],evaluate(expression[2],names))
        elif(expression[0] == 'set'):
            generator.set(evaluate(expression[1],names),evaluate(expression[2],names))
        elif(expression[0] == 'macro'):
            return Function(expression[1],expression[2::],names)
        else:
            func = evaluate(expression.pop(0),names)
            for item in expression:
                args.append(evaluate(item,names))
            return func(*args)
    
    else:
        try:
            return names[expression]
        except KeyError:
            return eval(expression)
    
def REPL():
    while(True):
        print("lispbrain> ",end='')
        inp = input()
        result = evaluate(splitNested(inp),standard)
        if(result is not None):
            print(result)

def compileFiles(inpFile,outFile):
    with open(inpFile,'r') as f:
        contents = f.read()
    run(contents,standard)
    with open(outFile,'w') as f:
        f.write(generator.code)

if(__name__ == '__main__'):
    parser = argparse.ArgumentParser(description='A lisp that compiles to brainfuck!')
    parser.add_argument('-i',help='input file')
    parser.add_argument('-o',help='output file')
    args = parser.parse_args()
    compileFiles(args.i,args.o)
