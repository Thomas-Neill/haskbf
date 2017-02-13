'''
Generate brainfuck code given parameters
'''
def genPrint(string):
    final = '[<]' #go until we hit a 0
    for char in string:
        final += '+'*ord(char) + '.[-]'
    return(final)
funcs = {'print':genPrint}
