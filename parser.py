'''
parse functions
'''
import re
from generators import funcs
NORMAL,LOOP = range(2)

def parse(line):
    '''
    returns [status,function,arglist]
    '''
    final = [NORMAL,None,()]
    functionName = re.match(r'\w+',line).group()
    print("FUNCTION {}".format(functionName))
    args = tuple(re.findall(r'[(,]([^,)]+)',line))
    print(args)
parse('foo("bar","BAZ!",27)')
