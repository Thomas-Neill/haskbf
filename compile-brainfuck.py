#!/usr/bin/env python3
import subprocess
import os
import sys
def convertBrainfuck(brainfuck):
    final = '#include <curses.h>\n#include <iostream>\n#include <vector>\nint main() {std::vector<char> stack {0};int pointer {0};'
    decoder = {'+': 'stack[pointer]++;',
               '-': 'stack[pointer]--;',
               '<': 'pointer--;',
               '>': 'if(pointer == stack.size()) stack.push_back(0); pointer++;',
               '[':'while(stack[pointer]) {',
               ']':'}',
               '.':'std::cout << stack[pointer];',
               ',':'stack[pointer] = getch();'
              }
    for char in brainfuck:
        if(char in decoder.keys()):
            final += decoder[char]
    return(final + '}')
def compileCPP(code):
    with open('temp.cpp','w') as f:
        f.write(code)
    subprocess.run(['g++-6','-o',sys.argv[2],'-lcurses','temp.cpp'])
    os.unlink('temp.cpp')
if(__name__ == '__main__'):
    with open(sys.argv[1]) as f:
        brainfuck = convertBrainfuck(f.read())
    compileCPP(brainfuck)
               
