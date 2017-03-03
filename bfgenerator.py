#!/usr/bin/env python3
import subprocess
import string
from optimize import optimize
from brainfuck import brainfuck

log = lambda x: None

class MemClaim:

    def __init__(self,index,tp,genRef,length):
        self.index = index
        self.tp = tp
        self.genRef = genRef
        self.length = length

    def set(self,val):
        assert tp(val) == self.tp or tp(val) == PYTHONTYPES[self.tp]
        self.genRef.set(self,val)

    def __le__(self,other): #useful shorthand
        self.set(other)

    '''def __del__(self): #This is unstable.
        self.genRef.delete(self)'''

    def __len__(self):
        return self.length

    def __str__(self):
        return "{} MemClaim of size {} at location {}".format(NAMES[self.tp],self.length,self.index)

STR, = VALIDTYPES = tuple(range(1))
PYTHONTYPES = [str]
NAMES = ['string']

def tp(val):
    if(isinstance(val,MemClaim)):
       return val.tp
    else:
        return type(val)

def isClaim(val):
    return isinstance(val,MemClaim)

class BFGenerator:

    def __init__(self):
        self.allocated = [1] #the 1 is a accumulator
        self.code = ''
        self.cursor = 0 #the brainfuck cursor's location in the memory

#   =====MEMORY MANAGEMENT =====    #

    def alloc(self,size,tp): #returns a memory claim of type x and size s
        log("Allocating a {} with size {} at index {}".format(NAMES[tp],size,len(self.allocated)))
        assert tp in VALIDTYPES,"Invalid type"
        self.allocated.append(size)
        return MemClaim(len(self.allocated)-1,tp,self,size)

    def set(self,memRef,val):
        log("Setting {} to {}".format(memRef,val))
        assert isClaim(memRef),"not a memory reference"
        if(tp(val) == str):
            self.access(memRef)
            assert len(val) <= len(memRef),"too big"
            for r in range(memRef.length):
                self.code += '[-]>'
                self.cursor += 1
            self.toStart()
            self.access(memRef)
            for char in val:
                self.code += '+' * ord(char) + '>'
                self.cursor += 1
            self.toStart()

        elif(isClaim(val)):
            assert len(val) <= len(memRef),"too big"
            assert tp(val) == tp(memRef),"wrong type"
            MemOffset = sum([self.allocated[i] for i in range(memRef.index)])
            ValOffset = sum([self.allocated[i] for i in range(val.index)])
            self.access(memRef)
            for r in range(memRef.length):
                self.code += '[-]>'
                self.cursor += 1
            self.toStart()
            for offset in range(val.length):
                self.copy(MemOffset+offset,ValOffset+offset)

    def access(self,ind): #go to a memory claim
        if(tp(ind) == int):
            log("Accessing memory at index {}".format(ind))
            for i in range(ind):
                self.go(self.allocated[i])

        elif(isClaim(ind)):
            self.access(ind.index)

    def delete(self,ind):
        
        if(tp(ind) == int):
            print(self.allocated,ind)
            log("Deleting memory at index {}".format(ind))
            length = self.allocated[ind]
            self.access(ind)
            for r in range(length):
                self.code += '[-]>'
                self.cursor += 1
            self.toStart()
            rng = sum([item for i,item in enumerate(self.allocated) if i>ind])
            toLength = sum(self.allocated) - rng - length
            for i in range(rng): #we have to move everything back after the erased memory
                self.copy(toLength+i,toLength+length+i)
            self.allocated.pop(ind)


        elif(isClaim(ind)):
            self.delete(ind.index)

#   ===== PRIMITIVE OPERATIONS =====    #

    def go(self,ind):
        self.code += '>'*ind
        self.cursor += ind

    def toStart(self): #resets the cursor
        while(self.cursor):
            self.code += '<'
            self.cursor -= 1

    def copy(self,x,y): #copys y to x
        self.go(x)
        self.code += '[-]'
        # we reset x
        self.toStart()
        self.go(y)
        self.code += '['
        self.toStart()
        self.go(x)
        self.code += '+'
        self.toStart()
        self.code += '+'
        self.go(y)
        self.code += '-]'
        #first loop,copying y to x and the accumulator, while erasing y
        self.toStart()
        self.code += '['
        self.go(y)
        self.code += '+'
        self.toStart()
        self.code += '-]'
        #now we copy the accumulator to y, resetting the accumulator

#   ===== HIGH-LEVEL OPERATIONS =====   #

    def puts(self,strv):
        log("Outputting {}".format(strv))
        if(tp(strv) == str):
            for char in strv:
                self.code += '+' * ord(char) + '.[-]'

        elif(tp(strv) == STR):
            self.access(strv)
            for r in range(len(strv)):
                self.code += '.>'
                self.cursor += 1
            self.toStart()

    def concat(self,str1,str2):
        log("Concatenating {} and {}".format(str1,str2))
        if((tp(str1) == str or tp(str1) == STR) and tp(str2) == STR):
            new = self.alloc(len(str1)+len(str2),STR)
            new <= str1
            newLocation = sum([self.allocated[i] for i in range(new.index)])
            strLocation = sum([self.allocated[i] for i in range(str2.index)])
            for offset in range(len(str2)):
                self.copy(newLocation+len(str1)+offset,strLocation+offset)
            return new


        elif(tp(str1) == STR and tp(str2) == str):
            new = self.alloc(len(str1) + len(str2),STR)
            new <= str1
            self.access(new)
            self.go(len(str1))
            for char in str2:
                self.code += '+' * ord(char) + '>'
                self.cursor += 1
            self.toStart()
            return new

def runBrainfuck(code):
    code = str(code)
    subprocess.run(['brainfuck',code])

def buffer(string):
    for char in string:
        yield char
        
if(__name__ == '__main__'):
        
    f = open('log.txt','w')
    log = lambda text: f.write(text + '\n')
    nonjunk = lambda char: chr(char) if chr(char) in string.printable else ''
    inp = buffer('')
    log("----- PROGRAM COMPILING -----")
    
    generator = BFGenerator()
    var = generator.alloc(50,STR)
    var <= "world!"
    generator.puts(generator.concat("Hello ",var))
    
    log("----- TESTING... -----")
    ret = brainfuck(
        generator.code,
        inFunc = lambda: next(inp),
        outFunc = lambda char: f.write(nonjunk(char))
        )
    log("\n----- MEMORY -----")
    for ind,i in enumerate(ret):
        log("Memory {}: {}".format(ind,i))
    f.close()
