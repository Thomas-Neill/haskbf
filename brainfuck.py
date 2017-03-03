import getch
import re
import string

def output(char):
    char = chr(char)
    if(char in string.printable[:-3:]):
        print(char,end='',flush=True)
        
def brainfuck(code,outFunc=output,inFunc=getch.getch):
    code = re.sub(r'[^\+-\[\].,<>]','',code)
    iCursor,mPointer,mem = 0,0,[0]
    while(True):
        instruction = code[iCursor]
        if(instruction == '+'):
            mem[mPointer] += 1
        elif(instruction == '-'):
            mem[mPointer] -= 1
        elif(instruction == '<'):
            mPointer -= 1
        elif(instruction == '>'):
            mPointer += 1
            if(mPointer == len(mem)):
                mem.append(0)
        elif(instruction == '.'):
            outFunc(mem[mPointer])
        elif(instruction == ','):
            mem[mPointer] = inFunc()
        elif(instruction == '['):
            if(mem[mPointer]==0):
                queue = 1
                while(queue):
                    iCursor += 1
                    if(code[iCursor] == '['):
                        queue += 1
                    elif(code[iCursor] == ']'):
                        queue -= 1
                assert len(code) != iCursor,"Out of bounds"
        elif(instruction == ']'):
            if(mem[mPointer]!=0):
                queue = 1
                while(queue):
                    iCursor -= 1
                    if(code[iCursor] == ']'):
                        queue += 1
                    elif(code[iCursor] == '['):
                        queue -= 1
                    assert iCursor > 0,"Out of bounds"
        iCursor += 1
        if(iCursor == len(code)):
            return mem
