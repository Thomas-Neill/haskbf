import subprocess
class MemClaim:
    def __init__(self,index,tp,genRef,length):
        self.index = index
        self.tp = tp
        self.genRef = genRef
        self.length = length
    def set(self,val):
        assert type(val) == TYPEDICT[self.tp]
        self.genRef.set(self,val)

VALIDTYPES = ['str','!!memClaim!!']
PYTHONTYPES = [str,MemClaim]
TYPEDICT = dict(zip(VALIDTYPES,PYTHONTYPES))

class BFGenerator:
    def __init__(self):
        self.allocated = [1] #the 1 is a buffer
        self.code = ''
        self.cursor = 0 #the brainfuck cursor's location in the memory
        
    def access(self,ind):
        if(isinstance(ind,int)):
            for i in range(ind):
                for r in range(self.allocated[i]):
                    self.code += '>'
                    self.cursor += 1
        elif(isinstance(ind,MemClaim)):
            self.access(ind.index)
                
    def toStart(self):
        while(self.cursor):
            self.code += '<'
            self.cursor -= 1

    def alloc(self,size,typeStr):
        assert typeStr in VALIDTYPES,"Invalid type"
        self.allocated.append(size)
        return MemClaim(len(self.allocated)-1,typeStr,self,size)

    def set(self,memRef,val):
        self.access(memRef)
        if(isinstance(val,str)):
            assert len(val) <= memRef.length,"too big"
            for r in range(memRef.length):
                self.code += '[-]>'
                self.cursor += 1
            self.toStart()
            self.access(memRef)
            for char in val:
                self.code += '+' * ord(char) + '>'
                self.cursor += 1
            self.toStart()

    def puts(self,strv):
        if(isinstance(strv,str)):
            for char in strv:
                self.code += '+' * ord(char) + '.[-]'
        if(isinstance(strv,MemClaim)):
            assert strv.tp == 'str',"Invalid args"
            self.access(strv)
            for r in range(strv.length):
                self.code += '.>'
                self.cursor += 1
            self.toStart()

def brainfuck(code):
    code = str(code)
    subprocess.run(['brainfuck',code])

if(__name__ == '__main__'):
    generator = BFGenerator()
    var = generator.alloc(50,'str')
    var.set("Hello world!\n")
    generator.puts(var)
    print(generator.code)
    #brainfuck(generator.code)
